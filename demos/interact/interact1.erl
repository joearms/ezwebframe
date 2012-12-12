-module(interact1).
-export([start/1]).

start(Browser) -> running(Browser).

running(Browser) ->
    receive
	{Browser, {struct, [{entry,<<"input">>},{txt, Bin}]}} ->
	    Time = clock1:current_time(),
	    Browser ! [{cmd,append_div},{id,scroll},
		       {txt, list_to_binary([Time, " > ", Bin, "<br>"])}]
    end,
    running(Browser).
