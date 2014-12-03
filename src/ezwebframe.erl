-module(ezwebframe).

-export([start_link/2,
	 init/2,      
	 websocket_handle/3, 
	 terminate/3, websocket_terminate/3,
	 websocket_info/3,
	 append_div/3,
	 pre/1,
	 fill_div/3
	]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).

%% env has only one parameter - reserved for future expansion

-record(env, {dispatch}).

start_link(Dispatch, Port) ->
    io:format("Starting:~p~n",[file:get_cwd()]),
    ok = application:start(crypto),
    ok = application:start(ranch), 
    ok = application:start(cowlib), 
    ok = application:start(cowboy),
    ok = web_server_start(Port, Dispatch).

web_server_start(Port, Dispatcher) ->
    E0 = #env{dispatch=Dispatcher},
    Dispatch = cowboy_router:compile([{'_',[{'_', ?MODULE, E0}]}]),  
    %% server is the name of this module
    NumberOfAcceptors = 100,
    Status = 
	cowboy:start_http(ezwebframe,
			  NumberOfAcceptors,
			  [{port, Port}],
			  [{env, [{dispatch, Dispatch}]}]),
    case Status of
	{error, _} ->
	    io:format("websockets could not be started -- "
		      "port ~p probably in use~n", [Port]),
	    init:stop();
	{ok, _Pid} ->
	    io:format("websockets started on port:~p~n",[Port])
    end.

init(Req, E0) ->   
    %% io:format("init:~n"),
    Resource = path(Req),
    %% io:format("Resource:~p~n",[Resource]),
    case Resource of
	["/", "websocket",ModStr] ->
	    Self = self(),
	    Mod = list_to_atom(ModStr),
	    %% Spawn an erlang handler
	    %% The return value will cause cowboy
	    %% to call this module at the entry point
	    %% websocket_handle
	    Pid = spawn_link(Mod, start, [Self]),
	    {cowboy_websocket, Req, Pid};
	_ ->
	    handle(Req, E0)
    end.

handle(Req, Env) ->
    Resource = filename:join(path(Req)),
    io:format("ezwebframe:handle ~p~n",[Resource]),
    F = Env#env.dispatch,
    Res1 = F(Resource),
    io:format("mapped to:~p~n",[Res1]),
    case Resource of
	"/" ->
	    serve_file("index.html", Req, Env);
	"/files" ->
	    list_dir(F("/"), Req, Env);
	_ ->
	    serve_file(Res1, Req, Env)
    end.

serve_file(File, Req, Env) ->
    case filelib:is_dir(File) of
	true ->
	    list_dir(File, Req, Env);
	false ->
	    serve_abs_file(File, Req, Env)
    end.

serve_abs_file(File, Req, Env) ->
    Val = file:read_file(File),
    case Val of 
	{error, _} ->
	    io:format("*** no page called ~p~n",[File]),
	    reply_html(pre({no_page_called,File}), Req, Env);
	{ok, Bin} ->
	    Ext  = filename:extension(File),
	    Bin1 = if_erlang_add_pre(Ext, Bin),
	    Req1 = send_page(classify_extension(Ext), Bin1, Req),
	    {ok, Req1, Env}
    end.

if_erlang_add_pre(".erl", B) -> ["<pre>", B, "</pre>"];
if_erlang_add_pre(_, B)      -> B.

list_dir(Root, Req, Env) ->
    {ok, Files} = file:list_dir(Root),
    Files1 = [add_slash(I, Root) || I <- Files],
    L1 = [["<li><a href='",I,"'>",I,"</a></li>\n"] || I <- lists:sort(Files1)],
    reply_html(["<h1> Directory ",Root, "</h1>\n",
		"<ul>\n",L1,"</ul>\n"], Req, Env).

add_slash(I, Root) ->
    Full = filename:join(Root, I),
    case filelib:is_dir(Full) of
	true  -> I ++ "/";
	false -> I
    end.

send_page(Type, Data, Req) ->
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).


path(Req) ->
    Path = cowboy_req:path(Req),
    filename:split(binary_to_list(Path)).

reply_html(Obj, Req, Env) ->
    Req1 = send_page(html, Obj, Req),
    {ok, Req1, Env}.

%%----------------------------------------------------------------------
%% other callbacks

terminate(_Reason,_Req,_State) ->
    %% ignore why we terminate
    ok.

%%----------------------------------------------------------------------
%% websocket stuff

websocket_handle({text, Msg}, Req, Pid) ->
    %% This is a Json message from the browser
    case catch decode(Msg) of
	{'EXIT', _Why} ->
	    Pid ! {invalidMessageNotJSON, Msg};
	{struct, _} = Z ->
	    X1 = atomize(Z),
	    Pid ! {self(), X1};
	Other ->
	    Pid ! {invalidMessageNotStruct, Other}
    end,
    {ok, Req, Pid}.

websocket_info({send,Str}, Req, Pid) ->
    {reply, {text, Str}, Req, Pid, hibernate};
websocket_info([{cmd,_}|_]=L, Req, Pid) ->
    B = list_to_binary(encode([{struct,L}])),
    {reply, {text, B}, Req, Pid, hibernate};
websocket_info(Info, Req, Pid) ->
    io:format("Handle_info Info:~p Pid:~p~n",[Info,Pid]),
    {ok, Req, Pid, hibernate}.

websocket_terminate(_Reason, _Req, Pid) ->
    io:format("websocket.erl terminate:~n"),
    exit(Pid, socketClosed),
    ok.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

%%----------------------------------------------------------------------
%% atomize turns all the keys in a struct to atoms

atomize({struct,L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.

%%----------------------------------------------------------------------
%% these are called from the gui client code

append_div(Ws, Div, X) ->
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,append_div},{id,Div},{txt,Bin}]).

fill_div(Ws, Div, X) ->
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,fill_div},{id,Div},{txt,Bin}]).
    
send_websocket(Ws, X) ->
    Ws ! {send, list_to_binary(encode([{struct,X}]))}.


%%----------------------------------------------------------------------
%% Miscellaneous small functions

classify_extension(".gif") -> gif;
classify_extension(".jpg") -> jpg;
classify_extension(".png") -> png;
classify_extension(".js")  -> js;
classify_extension(".css") -> css;
classify_extension(_)      -> html.

mime_type(gif)     -> "image/gif";
mime_type(jpg)     -> "image/jpeg";
mime_type(png)     -> "image/png";
mime_type(css)     -> "text/css";
mime_type(special) -> "text/plain; charset=x-user-defined";
mime_type(json)    -> "application/json";
mime_type(swf)     -> "application/x-shockwave-flash";
mime_type(html)    -> "text/html";
mime_type(xul)     -> "application/vnd.mozilla.xul+xml";
mime_type(js)      -> "application/x-javascript";
mime_type(svg)     -> "image/svg+xml".

pre(X) ->
    ["<pre>\n",quote(lists:flatten(io_lib:format("~p",[X]))), "</pre>"].

%% quote HTML characters "<" and "&" 

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].
