#!/usr/bin/env escript
%% -*- erlang -*-
%% @doc EDoc generation script.

main(_Args) ->
    {ok, Cwd} = file:get_cwd(),
    code:add_patha(filename:join([Cwd, "ebin"])),
    Files = [filename:join([Cwd, "src", "minino_api.erl"])],
    Dir = filename:join([Cwd, "doc"]),
    edoc:files(Files, [{dir, Dir},
		       {new, true},
		       {preprocess, true},
		       {includes, ["include"]}]).
