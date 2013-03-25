all:
	./rebar get-deps
	./rebar compile
	./bootstrap

clean:
	./rebar clean

test: 	all
	./rebar ct skip_deps=true

