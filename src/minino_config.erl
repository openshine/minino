%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  12 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_config).

-export([read/0]).


read()->
    Filename = 
	case application:get_env(minino, settings_file) of
	    {ok, P} -> P;
	    _ -> filename:join(["priv", "settings.cfg"])
	end,
    file:consult(Filename).


