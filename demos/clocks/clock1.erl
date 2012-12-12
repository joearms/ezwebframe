-module(clock1).
-export([start/1, current_time/0]).
%%% NOTE: lines with three %%% show code when frames are introduced

start(Browser) ->
    Browser ! [{cmd,fill_div}, {id,clock}, {txt,current_time()}],
    running(Browser).

running(Browser) ->
    receive
	{Browser, {struct, [{clicked,<<"stop">>}]}} ->
	    Browser ! [{cmd,fill_div}, {id,clock}, {txt,<<"Stopped">>}],
	    idle(Browser)
    after 1000 ->
	    Browser ! [{cmd,fill_div}, {id,clock}, {txt,current_time()}],
	    running(Browser)
    end.

idle(Browser) ->
    receive
	{Browser, {struct, [{clicked,<<"start">>}]}} ->
	    Browser ! [{cmd,fill_div}, {id,clock}, {txt,<<"Starting">>}],
	    running(Browser)
    end.

current_time() ->
    {Hour,Min,Sec} = time(),
    list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
				 [Hour,Min,Sec])).

