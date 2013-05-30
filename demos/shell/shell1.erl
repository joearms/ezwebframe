-module(shell1).
-export([start/1]).

%%START:shell1
start(Browser) ->
    Browser ! [{cmd,append_div}, {id, scroll}, 
	       {txt, <<"Starting Erlang shell:<br>">>}],
    B0 = erl_eval:new_bindings(),
    IO = grab_io(),
    running(Browser, IO, B0, 1).

running(Browser, IO, B0, N) ->
    receive
	{Browser, {struct, [{entry,<<"input">>},{txt, Bin}]}} ->
	    Echo = bf("~w > <font color='red'>~s</font><br>", [N, Bin]),
	    Browser ! [{cmd,append_div},{id, scroll}, {txt, Echo}],
	    {Value, B1} = string2value(binary_to_list(Bin), B0),
	    BV = bf("~p<br>", [Value]),
	    Browser ! [{cmd,append_div},{id, scroll}, {txt, BV}],
	    running(Browser, IO, B1, N+1);
	{IO, {output, Bin}} ->
	    Browser ! [{cmd, append_div}, {id,scroll},
		       {txt, <<"<pre>", Bin/binary, "</pre><br>">>}],
	    running(Browser, IO, B0, N)
    end.
%%END:shell1

%%START:shell2
string2value(Str, Bindings0) ->
    case erl_scan:string(Str, 0) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Exprs} -> 
                    {value, Value, Bindings1} = erl_eval:exprs(Exprs, Bindings0),
                    {Value, Bindings1};
                Other ->
                    io:format("cannot parse:~p Reason=~p~n",[Tokens,Other]),
		    {parse_error, Bindings0}
            end;
        Other ->
            io:format("cannot tokenise:~p Reason=~p~n",[Str,Other])
    end.
%%END:shell2

bf(F, D) ->
    list_to_binary(io_lib:format(F, D)).

grab_io() ->
    P = self(),
    spawn(fun() ->
		  group_leader(self(), P),
		  _ = erlang:monitor(process, P),
		  io_loop(P)
	  end).

io_loop(P) ->
    receive
	{io_request,From,ReplyAs,Req} when is_pid(From) ->
	    Res = io_request(Req, P),
	    io_reply(From, ReplyAs, Res),
	    io_loop(P);
	{'DOWN',_,process,P,_} ->
	    exit(normal)
    end.

io_request({put_chars, unicode, Binary}, P) when is_binary(Binary) ->
    output(P, Binary);
io_request({put_chars, unicode, M, F, As}, P) ->
    case catch apply(M, F, As) of
        Binary when is_binary(Binary) ->
	    output(P, Binary);
        Chars ->
            case catch unicode:characters_to_binary(Chars,utf8) of
                B when is_binary(B) ->
                    output(P, B);
                _ ->
                    {error,{error,F}}
            end
    end;
io_request({put_chars, latin1, Chars}, P) ->
    output(P, unicode:characters_to_binary(Chars, latin1));
io_request({put_chars, latin1, M, F, As}, P) ->
    case catch apply(M, F, As) of
        Binary when is_binary(Binary) ->
            output(P, unicode:characters_to_binary(Binary,latin1)),
	    ok;
        Chars ->
            case catch unicode:characters_to_binary(Chars,latin1) of
                B when is_binary(B) ->
		    output(P, B);
                _ ->
                    {error,{error,F}}
            end
    end;
io_request(_, _) ->
    {error, not_supported}.

output(P, Cs) ->
    P ! {self(), {output, Cs}},
    ok.

io_reply(From, ReplyAs, Reply) ->
    From ! {io_reply,ReplyAs,Reply}.


