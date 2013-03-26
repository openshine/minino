%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  12 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_config).

-export([get/0]).


get()->
    Filename = 
	case application:get_env(minino, settings_file) of
	    {ok, S} -> S;
	    _ -> filename:join(["priv", "settings.cfg"])
	end,
    {ok, Conf} = file:consult(Filename),

    Conf2 = 
    	case application:get_env(minino, mport) of
    	    {ok, P} -> 
		C = proplists:delete(port, Conf),
		[{port, P}|C];
	    _ -> Conf
    	end,

    TemplatesDir = 
    	case application:get_env(minino, templates_dir) of
	    {ok, T} -> 
		{templates_dir, T};
	    _ ->
		{templates_dir,
		 filename:join(["priv", "templates"])}
	end,
    {ok, [TemplatesDir|Conf2]}.


