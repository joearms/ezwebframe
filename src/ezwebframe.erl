-module(ezwebframe).

-export([start_link/1,
	 start_link/2,
	 start_embedded/1,
	 init/3,      websocket_init/3,
	 handle/2,    websocket_handle/3, 
	 terminate/2, websocket_terminate/3,
	 websocket_info/3,
	 append_div/3,
	 pre/1,
	 fill_div/3,
	 start_if_not_running/1
	]).

-import(ezwebframe_mochijson2, [encode/1, decode/1]).


%% env has only one parameter - reserved for future expansion

-record(env, {dispatch}).

start_embedded(Port) ->
    ok   = start_if_not_running(ranch),
    ok   = start_if_not_running(cowboy),
    web_server_start(Port, "zip"),
    receive
	after 
	    infinity ->
		true
	end.

start_link([PortAtom, DirAtom]) ->
    Port = list_to_integer(atom_to_list(PortAtom)),
    Dir  = atom_to_list(DirAtom),
    io:format("Starting server on port:~p Dir:~p~n",[Port,Dir]),
    start_link(Dir, Port).

start_link(Dispatch, Port) ->
    ok = start_if_not_running(crypto),
    ok = start_if_not_running(ranch),  
    ok = start_if_not_running(cowboy),
    ok = web_server_start(Port, Dispatch),
    receive
	after 
	    infinity ->
		true
	end.
    
web_server_start(Port, Dispatcher) ->
    E0 = #env{dispatch=Dispatcher},
    Dispatch = [{'_', [{'_', ?MODULE, E0}]}],  
    %% server is the name of this module
    NumberOfAcceptors = 100,
    Status = 
	cowboy:start_http(my_named_thing,
			  NumberOfAcceptors,
			  [{port, Port}],
			  [{dispatch, Dispatch}]),
    case Status of
	{error, _} ->
	    io:format("websockets could not be started -- "
		      "port ~p probably in use~n", [Port]),
	    init:stop();
	{ok, _Pid} ->
	    io:format("websockets started on port:~p~n",[Port])
    end.

init(_, Req, E0) ->   
    Resource = path(Req),
    io:format("init Resource =~p Env=~p~n",[Resource, E0]),
    case Resource of
	["/", "websocket",_] ->
	    %% The upgrade return value will cause cowboy
	    %% to call this module at the entry point
	    %% websocket_init
	    io:format("upgrading:~n"),
	    {upgrade, protocol, cowboy_websocket};
	_ ->
	    {ok, Req, E0}
    end.

terminate(_, _) ->  
    ok.
    
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
    io:format("serve_abs:~p~n",[File]),
    Val = file:read_file(File),
    case Val of 
	{error, _} ->
	    io:format("*** no page called ~p~n",[File]),
	    reply_html(pre({no_page_called,File}), Req, Env);
	{ok, Bin} ->
	    Ext = filename:extension(File),
	    Bin1 = add_wrapper(Ext, Bin),
	    {ok, Req1} = send_page(classify_extension(Ext), Bin1, Req),
	    {ok, Req1, Env}
    end.

add_wrapper(".erl", B) ->
    ["<pre>", B, "</pre>"];
add_wrapper(_, B) ->
    B.


list_dir(Root, Req, Env) ->
    io:format("List dir:~p~n",[Root]),
    {ok, Files} = file:list_dir(Root),
    Files1 = [add_slash(I, Root) || I <- Files],
    L1 = [["<li><a href='",I,"'>",I,"</a></li>\n"] || I <- lists:sort(Files1)],
    reply_html(["<h1> Directory ",Root, "</h1>\n",
		"<ul>\n",L1,"</ul>\n"], Req, Env).

add_slash(I, Root) ->
    io:format("Add slash:~p ~p~n",[I,Root]),
    Full = filename:join(Root, I),
    case filelib:is_dir(Full) of
	true ->
	    I ++ "/";
	false ->
	    I
    end.

send_page(Type, Data, Req) ->
    cowboy_req:reply(200, [{<<"Content-Type">>,
			    list_to_binary(mime_type(Type))}],
		     Data, Req).

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

quote("<" ++ T) -> "&lt;" ++ quote(T);
quote("&" ++ T) -> "&amp;" ++ quote(T);
quote([H|T])    -> [H|quote(T)];
quote([])       -> [].

path(Req) ->
    {Path,_} = cowboy_req:path(Req),
    P = filename:split(binary_to_list(Path)),
    io:format("Path=~p~n",[P]),
    P.

%% args(Req) ->
%%     {Args, _} = cowboy_req:qs_vals(Req),
%%     Args.

%% reply_type(Type, Data, Req, Env) ->
%%     {ok, Req1} = send_page(Type, Data, Req),
%%     {ok, Req1, Env}.

reply_html(Obj, Req, Env) ->
    {ok, Req1} = send_page(html, Obj, Req),
    {ok, Req1, Env}.

%%----------------------------------------------------------------------
%% websocket stuff

websocket_init(_Transport, Req, _Env) ->
         io:format("Initialising a web socket:(~p)(~p)(~p)",
		   [_Transport, _Env, path(Req)]),
    ["/", "websocket", ModStr] = path(Req),
    %% Args = args(Req),
    Req1 = cowboy_req:compact(Req),
    Self = self(),
    Mod = list_to_atom(ModStr),
    %% Spawn an erlang handler
    Pid = spawn_link(Mod, start, [Self]),
    {ok, Req1, Pid, hibernate}.

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

%% reply_json(Obj, Req, Env) ->
%%     %% Encode Obj as JSON and send to the browser
%%     Json = encode(Obj),
%%     {ok, Req1} = send_page(json, Json, Req),
%%     {ok, Req1, Env}.

binary_to_atom(B) ->
    list_to_atom(binary_to_list(B)).

%% rpc(Pid, M) ->
%%     S = self(),
%%     Pid ! {S, M},
%%     receive
%% 	{Pid, Reply} ->
%% 	    Reply
%%     end.

%%----------------------------------------------------------------------
%% atomize turns all the keys in a struct to atoms

atomize({struct,L}) ->
    {struct, [{binary_to_atom(I), atomize(J)} || {I, J} <- L]};
atomize(L) when is_list(L) ->
    [atomize(I) || I <- L];
atomize(X) ->
    X.

start_if_not_running(App) ->
    case application:get_application(App) of
	{ok, App} -> ok;
	undefined -> application:start(App)
    end.
%%----------------------------------------------------------------------
%% these are to be called from the gui client code

append_div(Ws, Div, X) ->
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,append_div},{id,Div},{txt,Bin}]).

fill_div(Ws, Div, X) ->
    io:format("websockets X=~p~n",[X]),
    Bin = list_to_binary(X),
    send_websocket(Ws, 
		   [{cmd,fill_div},{id,Div},{txt,Bin}]).
    

send_websocket(Ws, X) ->
    Ws ! {send, list_to_binary(encode([{struct,X}]))}.



