%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino).

-export([main/1,
	 start/0,
	 stop/0]).

%% script
main(Args) ->
    minino_escript:main(Args).

start()->
    application:start(?MODULE).

stop()->
    application:stop(?MODULE).
