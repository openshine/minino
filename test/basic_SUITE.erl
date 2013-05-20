-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(BLOCKSIZE, 32768).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([simple_tests/1, 
	 escript_tests/1,
	 kitty_tests/1,
	 hello_world_example_tests/1,
	 upload_file_example_tests/1,
	 miscellaneous_example_tests/1,
	 sessions_example_tests/1,
	 statics_example_tests/1,
	 templates_example_tests/1
	]).

all() -> [templates_example_tests,
	  statics_example_tests,
	  sessions_example_tests,
	  miscellaneous_example_tests,
	  upload_file_example_tests,
	  hello_world_example_tests,
	  kitty_tests,
	  escript_tests, 
	  simple_tests
	 ].




init_per_testcase(templates_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(templates),   
   
    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([ExampleDir, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      

    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;


init_per_testcase(statics_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(statics),   
   
    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([ExampleDir, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      


    %% over write media_path 
    Path = filename:join([ExampleDir, "priv", "statics"]),
    application:set_env(minino, media_path, Path),    
    

    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;


init_per_testcase(sessions_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(sessions),   
   
    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([ExampleDir, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      
      
    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;



init_per_testcase(miscellaneous_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(miscellaneous),   
   
    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([ExampleDir, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      
      
    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;


init_per_testcase(upload_file_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(upload_file),   

    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([ExampleDir, "priv", "templates"]),
    application:set_env(minino, templates_dir, Templates),      

    %% over write file_name 
    TestPrivDir = proplists:get_value(priv_dir, Config),
    FileName = filename:join([TestPrivDir, "tmp", "filename"]),
    application:set_env(minino, file_name, FileName),      
      
    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;


init_per_testcase(hello_world_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(hello_world),

    %% set settings.cfg file
    Settings = filename:join([ExampleDir, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% start inets
    application:start(inets),

    %%start minino
    minino:start(),
    Config;  



init_per_testcase(kitty_tests, Config) ->
    %%compile app
    DataDir = proplists:get_value(data_dir, Config),
    AppSource = filename:join([DataDir, "kitty.erl"]),
    {ok, kitty} = compile:file(AppSource),

    %% set settings.cfg file
    Settings = filename:join([DataDir, "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      

    %% set templates dir
    Templates = filename:join([DataDir, "templates"]),
    application:set_env(minino, templates_dir, Templates),      

    %% start inets
    application:start(inets),

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
    error_logger:info_msg("end test~n"),
    ok.

%%======================================================
%% templates_example_tests
%%======================================================

templates_example_tests(_Config)->
    Url = "http://127.0.0.1:8000",
    {200, _Body1, SessionKey} = request(Url),
    ok.

%%======================================================
%% statics_example_tests
%%======================================================

statics_example_tests(_Config)->
    Url = "http://127.0.0.1:8000",
    {200, _Body1, SessionKey} = request(Url),
    Url1 = Url ++ "/media/text.txt",
    {200, _Body2, SessionKey} = request(Url1, SessionKey),
    ok.

%%======================================================
%% sessions_example_tests
%%======================================================

sessions_example_tests(_Config)->
    Url = "http://127.0.0.1:8000/",
    {200, _Body1, SessionKey} = request(Url),
    Dict1 = minino_api:get_session_dict(SessionKey),
    error = dict:find("key", Dict1),
    {200, _Body2, SessionKey} = request(Url ++ "?value=minino", SessionKey),
    Dict2 = minino_api:get_session_dict(SessionKey),
    {ok, "minino"} = dict:find("key", Dict2),
    {200, _Body3, SessionKey} = request(Url ++ "?value=minino1", SessionKey),
    Dict3 = minino_api:get_session_dict(SessionKey),
    {ok, "minino1"} = dict:find("key", Dict3),
    ok.


%%======================================================
%%  miscellaneous_example_tests
%%======================================================

miscellaneous_example_tests(_Config)->
    Url = "http://127.0.0.1:8000/",
    {200, _Body, _} = request(Url),
    error_logger:info_msg("code 200 -> ok~n"),
    ok = miscellaneous_server:test(),
    ok.

%%======================================================
%% upload_file_example_tests 
%%======================================================


upload_file_example_tests(Config)->
    Url = "http://127.0.0.1:8000/upload",
    {200, _Body, _} = request(Url),
    error_logger:info_msg("code 200 -> ok~n"),
    DataDir = proplists:get_value(data_dir, Config),
    SourcePath = filename:join([DataDir,"test_files", "logo.png"]),
    post_file(Url, SourcePath),
    {ok, MConf} = minino_api:get_conf(),
    DestinationPath = proplists:get_value(file_name, MConf), 
    DestinationMd5 =  get_file_md5(DestinationPath),
    SourceMd5  = get_file_md5(SourcePath),
    if 
	DestinationMd5 == SourceMd5 ->
	    ok
    end,
    ok.

post_file(Url, Path)->
    {ok, Data} = file:read_file(Path),
    httpc:request(post, {Url, [], "image/png", Data}, [], []),
    ok.


%%======================================================
%% hello_world_example_tests
%%======================================================


hello_world_example_tests(_Config)->
    Url = "http://127.0.0.1:8000",
    {200, _Body, _} = request(Url),
    error_logger:info_msg("code 200 -> ok~n"),
    ok.



%%======================================================
%% kitty_tests
%%======================================================

kitty_tests(_Config)->
    kitty = kitty:check_module(),
    Url = "http://127.0.0.1:8000",
    error_logger:info_msg("check http codes~n"),
    {200, _Body, _} = request(Url),
    error_logger:info_msg("code 200 -> ok~n"),
    {404, _Body1, _} = request(Url ++ "/undefined"),
    error_logger:info_msg("code 404 -> ok~n"),
    {500, _Body2, _} = request(Url ++ "/error500"),
    error_logger:info_msg("code 500 -> ok~n"),
    ok.
%%======================================================
%% escript_tests
%%======================================================

escript_tests(_Config) ->
    Url = "http://127.0.0.1:8000",
    {200, _Body, _} = request(Url),
    error_logger:info_msg("test ping: ~p -> ok", [Url]),
    Cookie = check_cookies(Url),
    error_logger:info_msg("check cookies: ~p~n", [Cookie]),
    ok.

check_cookies(Url) ->
    {ok, R} = httpc:request(Url),
    {{_,200,_},Headers,_Body} = R,
    {ok, Cookie} = get_session(Headers),
    error_logger:info_msg("cookie: ~p~n", [Cookie]),
    Request = {Url, [{"Cookie", "msession=" ++ Cookie}]},
    {ok, R1} = httpc:request(get, Request, [], []),
    {{_,200,_},Headers1,_Body1} = R1,
    error = get_session(Headers1),
    Cookie.


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
    error_logger:info_msg("~n** dispatch rules test **"),
    DRules = get_dispatch_rules(), 

    error_logger:info_msg("Dispatch rules: ~p", [DRules]),
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
    error_logger:info_msg("check path ~p Expected: ~p -> ok", [Path, Expected]).



check_build_urls() ->
    DRules = get_dispatch_rules(), 
    F = minino_dispatcher:create_build_url_fun(DRules),
    "/" = F(root_page, []),
    "/home" = F(home_page, []),
    "/test/data" = F(test_page, [{testvalue, "data"}]),
    ok.


get_file_md5(CompletePath)->
    case file:open(CompletePath, [binary,raw,read]) of
	{ok, P} -> 
	    case get_code_loop(P, erlang:md5_init()) of
		{ok, MD5} ->
		    {ok, md5_to_str(MD5)};
		Error ->
		    Error
	    end;
	Error -> 
	    Error
    end.


get_code_loop(File, C) ->
    case file:read(File, ?BLOCKSIZE) of
	{ok, Bin} ->
	    get_code_loop(File, erlang:md5_update(C, Bin));
	eof ->
	    file:close(File),
        {ok, erlang:md5_final(C)}
    end.

md5_to_str(MD5) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= MD5]).

compile_example_mods(Example) ->
    MininoPriv = code:priv_dir(minino),
    Name = erlang:atom_to_list(Example),
    ExampleDir = filename:join([MininoPriv, "..", "examples", Name]),
    Files = filelib:wildcard(filename:join([ExampleDir, "src", "*.erl"])),
    compile_mods_loop(Files, ExampleDir).
    
compile_mods_loop([], ExampleDir)->
     {ok, ExampleDir};

compile_mods_loop([File|Tail], ExampleDir)->
    {ok, _Mod} = compile:file(File),
    compile_mods_loop(Tail, ExampleDir).

request(Url) ->
    request(get, Url, undefined).

request(Url, Session) ->
    request(get, Url, Session).

request(Method, Url, Session) ->
    Headers = 
	case Session of
	    Undefined when Undefined == undefined ->
		[];
	    Session ->
		
		[{"Cookie", "msession=" ++ Session}]
	end,
    Request = {Url, Headers},
    {ok, {{_,Code,_}, ReceivedHeaders, Body}}= httpc:request(Method, Request, [{timeout, 3000}], []),
    NewSession =
	case get_session(ReceivedHeaders) of
	    error when Session /= undefined-> Session;
	    {ok, NewSess} -> NewSess
	end,
    {Code, Body, NewSession}.


get_session(Headers) ->
    case proplists:get_value("set-cookie", Headers) of
	undefined -> error;
	Val ->
	    get_session_loop(string:tokens(Val, "; "))
    end.

get_session_loop([])->
    error;

get_session_loop([H|T])->
    case string:tokens(H, "=") of
	["msession", Session] ->
	    {ok, Session};
	_Else ->
	    get_session_loop(T) 
    end.
