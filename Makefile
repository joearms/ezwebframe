all:
	@test -d deps || rebar get-deps	
	rebar compile
	cd demos; make

clean:
	rm -rf *~ *.beam *.tmp 
	rm -rf ebin/*~ ebin/*.beam ebin/*.tmp 
	rm -rf demos/ebin/*~ demos/ebin/*.beam demos/ebin/*.tmp
	rm -rf deps

