-module(irc).
-export([start/0]).

start() ->
    register(irc, spawn(fun() -> start1() end)).

start1() ->
    process_flag(trap_exit, true),
    loop([]).

loop(L) ->
    receive
	{join, Pid, Who} ->
	    case lists:keysearch(Who,1,L) of
		false ->
		    L1 = L ++ [{Who,Pid}],
		    Pid ! {irc, welcome, Who},            
		    Msg = [Who, <<" joined the chat<br>">>],
		    broadcast(L1, scroll, list_to_binary(Msg)),
		    broadcast(L1, groups, list_users(L1)),
		    loop(L1);
		{value,_} ->
		    Pid ! {irc, error, <<"Name taken">>},
		    loop(L)
	    end;
	{leave, Who} ->
	    case lists:keysearch(Who,1,L) of
		false ->
		    loop(L);
		{value,{Who,Pid}} ->
		    L1 = L -- [{Who,Pid}],
		    Msg = [Who, <<" left the chat<br>">>],
		    broadcast(L1, scroll, list_to_binary(Msg)),
		    broadcast(L1, groups, list_users(L1)),
		    loop(L1)
	    end;
	{broadcast, Who, Txt} ->
	    broadcast(L, scroll, 
		      list_to_binary([" > ", Who, " >> ", Txt, "<br>"])),
	    loop(L);
	X ->
	    io:format("irc:received:~p~n",[X]),
	    loop(L)
    end.

broadcast(L, Tag, B) ->
    [Pid ! {irc, Tag, B} || {_,Pid} <- L].

list_users(L) ->
    L1 = [[Who,"<br>"] || {Who,_}<- L],
    list_to_binary(L1).
