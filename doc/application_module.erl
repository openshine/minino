%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%% Minino application module
%%% <p>This is the application main module. The application code must be placed here.</p>
%%% <p>The <a href="#dispatch_rules-0">dispatch_rules</a> and the 
%%% <a href="#views-3">views</a>, among other functions, should be placed in this module.</p>
%%% @end
%%% Created :  22 May 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(application_module).

-export([init/1,
	 dispatch_rules/0,
	 views/3,
	 add_children_to_main_sup/1
	]).



%%%---------------------------------------------------------------------
%%% *** This module has been created only for documentation purpose ***
%%%---------------------------------------------------------------------


%% @doc Initialization function
%% <p>Term is not a state, it can not be updated.</p>
%% @end
-spec init(MConf::m_conf())-> {ok, Term::term()}.
init(_MConf) ->
    {ok, "term()"}.

%% @doc Dispatch rules
%% <p> This function returns a list of dispatch rules.</p>
%% <p>example:<br />
%% [<br />
%%     {home_page, "^/$", home_view}, <br />
%%     {home_page2, "^/home$", home_view}, <br />
%%     {test_page, "^/test/(\\w*)$", test_view} <br />
%%   ].</p> 
%% Path is a regular expression of erlang module re.
%% 
%% @end
-spec dispatch_rules()-> {Id::atom(), Regex::string(), View::atom()}.
dispatch_rules() ->
    ok.
    
%% @doc Views
%% <p>It can be created as many views functions as be needed. They must be exported.</p>
%% <p>When a dispatch rule matchs the associated view is executed.</p>
%% <p>Term is not a state, it can not be updated.</p>
%%
%% @end
-spec views(MReq::minino_req(), Args::[Value::string()], Term::term()) -> ok.
views(MReq, _Args, Term) ->
    ok.

%% @doc Add children to main supervisor.
%% <p>This is not a mandatory function. You can use it if you want to add another process (supervisor or worker) to
%% the minino main supervisor.</p>
%% @end
-spec add_children_to_main_sup(MConf::m_conf()) -> [child_spec()].
add_children_to_main_sup(_MConf) ->
    ok.
