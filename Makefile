all:
	./rebar get-deps
	./rebar compile
	bin/bootstrap

clean:
	./rebar clean

test: 	all
	./rebar ct skip_deps=true

doc:	all
	bin/create_edoc.sh
