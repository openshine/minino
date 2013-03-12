%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% start cowboy
    Dispatch = 
    	cowboy_router:compile([
    			       {'_', [{'_', minino_cowboy_handler, []} ]}
    			      ]),
    {ok, _} = 
    	cowboy:start_http(http, 
    			  100, 
    			  [{port, 8080}], 
    			  [{env, [{dispatch, Dispatch}]}]
    			 ), 
    minino_sup:start_link().

stop(_State) ->
    ok.
