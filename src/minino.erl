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
    case  application:start(?MODULE) of
	ok-> ok;
	{error,{already_started,_}} -> ok;
	Error -> Error
    end.
					   	   
stop()->
    application:stop(?MODULE).
