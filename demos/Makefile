## set the paths for a default setup

all: beam
	erl -pa ../deps/cowboy/ebin\
            -pa ../deps/ranch/ebin\
            -pa ../deps/cowlib/ebin\
            -pa ../ebin\
	    -pa ./ebin\
            -s ezwebframe_demos start

beam: 
	erlc -o ebin *.erl */*.erl

clean:
	rm -rf *~ *.beam *.tmp
