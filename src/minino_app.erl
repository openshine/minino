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
    %% read mapp conf
    {ok, MConf} = minino_config:read(), 

    %% start cowboy
    Port =  proplists:get_value(port, MConf, 8080),
    Dispatch = 
    	cowboy_router:compile([
    			       {'_', [{'_', minino_cowboy_handler, []} ]}
    			      ]),
    {ok, _} = 
    	cowboy:start_http(http, 
    			  100, 
    			  [{port, Port}], 
    			  [{env, [{dispatch, Dispatch}]}]
    			 ), 
    io:format("~nstart minino web server: http://127.0.0.1:~p~n", [Port]),
    Params = [MConf],
    minino_sup:start_link(Params).

stop(_State) ->
    ok.
