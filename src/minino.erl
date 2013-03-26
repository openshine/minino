%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino).


%%API
-export([main/1,
	 start/0,
	 stop/0]).

%% escript
main(Args) ->
    minino_escript:main(Args).

start()->
    ok = ensure_start(crypto),
    ok = ensure_start(ranch),
    ok = ensure_start(cowboy),
    ok = ensure_start(minino).  
					   	   
stop()->
    application:stop(?MODULE).


ensure_start(App) ->
    case  application:start(App) of
 	ok-> ok;
 	{error,{already_started,_}} -> ok;
 	Error ->
	    error_logger:error_msg("start app ~p: ~p", [App, Error]),
	    Error
     end.
