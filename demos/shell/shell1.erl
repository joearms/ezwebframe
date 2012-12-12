-module(shell1).
-export([start/1]).

%%START:shell1
start(Browser) ->
    Browser ! [{cmd,append_div}, {id, scroll}, 
	       {txt, <<"Starting Erlang shell yes<br>">>}],
    B0 = erl_eval:new_bindings(),
    running(Browser, B0, 1).

running(Browser, B0, N) ->
    receive
	{Browser, {struct, [{entry,<<"input">>},{txt, Bin}]}} ->
	    {Value, B1} = string2value(binary_to_list(Bin), B0),
	    BV = bf("~w > <font color='red'>~s</font><br>~p<br>", [N, Bin, Value]),
	    Browser ! [{cmd,append_div},{id, scroll}, {txt, BV}],
	    running(Browser, B1, N+1)
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

