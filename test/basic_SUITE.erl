-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([simple_tests/1, 
	 escript_tests/1,
	 kitty_tests/1
	]).

all() -> [
	  kitty_tests,
	  escript_tests, 
	  simple_tests
	 ].



init_per_testcase(kitty_tests, Config) ->
    application:start(inets),

    %%compile app
    DataDir = proplists:get_value(data_dir, Config),
    AppSource = filename:join([DataDir, "kitty.erl"]),
    {ok, kitty} = compile:file(AppSource),

    %% set settings.cfg file
    Settings = filename:join([DataDir, "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set settings.cfg file
    Templates = filename:join([DataDir, "templates"]),
    application:set_env(minino, templates_dir, Templates),      

    %%start minino
    minino:start(),
    Config;  


init_per_testcase(simple_tests, Config) ->
    Config;   


init_per_testcase(escript_tests, Config) ->
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
    minino:stop(),
    ok.



%%======================================================
%% kitty_tests
%%======================================================

kitty_tests(_Config)->
    kitty = kitty:check_module(),
    Url = "http://127.0.0.1:8000",
    io:format("check http codes~n"),
    200 = get_http_code(Url),
    io:format("code 200 -> ok~n"),
    404 = get_http_code(Url ++ "/undefined"),
    io:format("code 404 -> ok~n"),
    500 = get_http_code(Url ++ "/error500"),
    io:format("code 500 -> ok~n"),
    ok.
%%======================================================
%% escript_tests
%%======================================================

escript_tests(_Config) ->
    Url = "http://127.0.0.1:8000",
    200 = get_http_code(Url),
    io:format("test ping: ~p -> ok", [Url]),
    Cookie = check_cookies(Url),
    io:format("check cookies: ~p~n", [Cookie]),
    ok.

%% ping(Url) ->  
%%     {ok, R} =    httpc:request(get, {Url, []}, [{timeout, 3000}], []),
%%     {{_,200,_},_Headers,_Body} = R,
%%     ok.

get_http_code(Url) ->
    {ok, R} =    httpc:request(get, {Url, []}, [{timeout, 3000}], []),
    {{_,Code,_},_Headers,_Body} = R,
    Code.

check_cookies(Url) ->
    {ok, R} = httpc:request(Url),
    {{_,200,_},Headers,_Body} = R,
    {ok, Cookie} = get_cookie(Headers),
    io:format("cookie: ~p~n", [Cookie]),
    Request = {Url, [{"Cookie", "msession=" ++ Cookie}]},
    {ok, R1} = httpc:request(get, Request, [], []),
    {{_,200,_},Headers1,_Body1} = R1,
    error = get_cookie(Headers1),
    Cookie.

get_cookie(Headers) ->
    case proplists:get_value("set-cookie", Headers) of
	undefined -> error;
	Val ->
	  get_cookie_loop(string:tokens(Val, "; "))
    end.

get_cookie_loop([])->
    error;

get_cookie_loop([H|T])->
    case string:tokens(H, "=") of
	["msession", Cookie] ->
	    {ok, Cookie};
	_Else ->
	   get_cookie_loop(T) 
    end.

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
    "/" = F(root_page, []),
    "/home" = F(home_page, []),
    "/test/data" = F(test_page, [{testvalue, "data"}]),
    ok.
    
