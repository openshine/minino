all:
	./rebar get-deps
	./rebar compile
	./bootstrap

clean:
	./rebar clean
