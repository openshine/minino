%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_WITH_PARAMS(I, Type, Params), {I, {I, start_link, [Params]}, permanent, 5000, Type, [I]}).
%% ===================================================================
%% API functions
%% ===================================================================

start_link(Params) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Params).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
 
init(Params) ->
    [MConf] = Params,
    ConfServer = ?CHILD_WITH_PARAMS(minino_config, worker, Params),
    SessionsServer = ?CHILD_WITH_PARAMS(minino_sessions, worker, Params),
    DispSup = ?CHILD_WITH_PARAMS(minino_dispatcher_sup, supervisor, Params),
    TempSup = ?CHILD_WITH_PARAMS(minino_templates_sup, supervisor, Params),
    Specs = [ConfServer, SessionsServer, DispSup, TempSup],
    AppMod = proplists:get_value(app_mod, MConf),
    AppChildSpecs = 
    	try
    	   AppMod:add_children_to_main_sup(MConf)
    	catch _:E ->
		error_logger:info_msg("error: ~p~n", [E]),
    		[]
    	end,



    {ok, {{one_for_one, 5, 10}, Specs ++ AppChildSpecs}}.
