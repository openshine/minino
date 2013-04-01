-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([simple_tests/1, 
	 tests/1]).

all() -> [tests, 
	  simple_tests
	 ].

init_per_testcase(simple_tests, Config) ->
    Config;   

init_per_testcase(tests, Config) ->
    application:start(inets),

    MininoPriv = code:priv_dir(minino),
    MininoBinFilename = filename:join([MininoPriv, "..", "bin", "minino"]),

    TestPriv = proplists:get_value(priv_dir, Config),
    CpCmd = lists:flatten(io_lib:format("cp ~s ~s", [MininoBinFilename, TestPriv])),
    os:cmd(CpCmd),
    AppCmd = lists:flatten(
    	       io_lib:format("cd ~s; ./minino create-app id=kitty", [TestPriv])),
    os:cmd(AppCmd),

    KittyPath = filename:join([TestPriv, "src", "kitty.erl"]),
    compile:file(KittyPath),

    %% set settings.cfg file
    Settings = filename:join([TestPriv, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([TestPriv, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      
    
    %%start minino
    minino:start(),
    Config.

end_per_testcase(_Test, _Config) ->
    ok.



%%======================================================
%% tests
%%======================================================

tests(_Config) ->
    ping(),
    ok.

ping() ->  
    {ok, R} = httpc:request("http://127.0.0.1:8000"),
    {{_,200,_},_,_} = R,
    io:format("test ping: http://127.0.0.1:8000 -> ok"),
    ok.


%%======================================================
%% Simple Tests
%%======================================================

simple_tests(_Config) ->
    check_dispatch_rules(),
    check_build_urls(),
    ok.


get_dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view},
     {home_page, ["home"], home_view},
     {test_page, ["test", testvalue], test_view}
    ].

check_dispatch_rules() ->
    io:format("~n** dispatch rules test **"),
    DRules = get_dispatch_rules(), 

    io:format("Dispatch rules: ~p", [DRules]),
    F = minino_dispatcher:create_match_fun(DRules),

    %% test path
    P0 = [],
    check_path(P0, F, {home_view, []}),

    %% test path
    P1 = ["home"],
    check_path(P1, F, {home_view, []}),

    %% test path
    P2 = ["test", "data"],
    check_path(P2, F, {test_view, [{testvalue, "data"}]}),

    %% test path
    P3 = ["undefined"],
    check_path(P3, F, undefined),

    ok.

check_path(Path, Fun, Expected)->
    R =  Fun(Path),
    case Expected of
	{EView, EArgs} ->
	    {EView, RArgs}  = Fun(Path),
	    lists:foreach(
	      fun(E) -> true = lists:member(E, EArgs) end,
	      RArgs);
	undefined -> R = undefined
    end,
    io:format("check path ~p Expected: ~p -> ok", [Path, Expected]).



check_build_urls() ->
    DRules = get_dispatch_rules(), 
    F = minino_dispatcher:create_build_url_fun(DRules),
    io:format("~p path -> ~p", [root_page, F(root_page, [])]),
    io:format("~p path -> ~p", [home_page, F(home_page, [])]),
    io:format("~p path -> ~p", [test_page, F(test_page, [{testvalue, "data"}])]),

    ok.
    
