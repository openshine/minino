Minino Statics 
==============

Use rebar to compile this example. You can use the escript stored in the project main dir.
``` bash
$ ../../rebar get-deps compile
```
You need the minino escript, as well.
``` bash
$ cp ../../bin/minino .
```
Start minino
``` bash
$ ./minino runserver
```

You can start an erlang node and start the minino "statics" app without the minino escript typing the following command:

``` bash
erl -pa ebin/ -pa deps/*/ebin -s minino
```
