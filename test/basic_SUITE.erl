-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([simple_tests/1, 
	 test/1]).

all() -> [simple_tests].

init_per_testcase(simple_tests, Config) ->
    Config;   

init_per_testcase(test, Config) ->
    application:start(inets),

    MininoPriv = code:priv_dir(minino),
    MininoBinFilename = filename:join([MininoPriv, "..", "bin", "minino"]),
    io:format("MininoBinFilename: ~p~n", [MininoBinFilename]),
    TestPriv = proplists:get_value(priv_dir, Config),
    CpCmd = lists:flatten(io_lib:format("cp ~s ~s", [MininoBinFilename, TestPriv])),
    os:cmd(CpCmd),
    AppCmd = lists:flatten(
	       io_lib:format("cd ~s; ./minino create-app id=kitty; ./rebar get-deps compile", [TestPriv])),
    os:cmd(AppCmd),

    %% add ebin kitty to path
    code:add_path(filename:join(TestPriv, "ebin")),

    %% set settings.cfg file
    Settings = filename:join([TestPriv, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      
    
    %%start minino
    minino:start(),
    Config.

end_per_testcase(_Test, _Config) ->
    ok.



%%======================================================
%% TESTS
%%======================================================

test(_Config) ->
    {ok, _} = httpc:request("http://127.0.0.1:8000"),
    ok.



simple_tests(_Config) ->
    check_dispatch_rules(),
    ok.
    







check_dispatch_rules() ->
    io:format("check_dispatch_rules~n"),
    DRules =    
	[%% {Id::atom(), Path::[string()|atom()], view::atom()}
	 {root_page, [], home_view},
	 {home_page, ["home"], home_view},
	 {test_page, ["test", testvalue], test_view}
	],
    io:format("Dispatch rules: ~p~n", [DRules]),
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
    io:format("check path ~p Expected: ~p -> ok~n", [Path, Expected]).

