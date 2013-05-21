#!/usr/bin/env escript
%% -*- erlang -*-
%% @author Pablo Vieytes <pvieytes@openshine.com>
%% @copyright (C) 2013, Openshine s.l.
%% @doc
%% EDoc generation script.
%% @end
%% Created :  22 May 2013 by Pablo Vieytes <pvieytes@openshine.com>


-define(ALL, false).
-define(FILES, ["minino_api.erl"]).


main(_Args) ->
    {ok, Cwd} = file:get_cwd(),
    code:add_patha(filename:join([Cwd, "ebin"])),
    code:add_patha(filename:join([Cwd, "include"])),
    Files = get_files(Cwd),
    Dir = filename:join([Cwd, "doc"]),
    Opts = [{dir, Dir},
	    {new, true}
	   ],
    edoc:files(Files, Opts).    





get_files(Cwd) ->
    CodeFiles=
	case ?ALL of
	    false ->
		[filename:join([Cwd, "src", File]) || File <- ?FILES];
	    true ->
		filelib:wildcard(filename:join([Cwd, "src", "minino*.erl"]))
	end,
    ExtraFiles = filelib:wildcard(filename:join([Cwd, "doc", "*.erl"])),
    CodeFiles ++ ExtraFiles.
       
