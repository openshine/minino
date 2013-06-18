-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-define(BLOCKSIZE, 32768).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([
	 url_tests/1,
	 escript_tests/1,
	 hello_world_example_tests/1,
	 upload_file_example_tests/1,
	 miscellaneous_example_tests/1,
	 sessions_example_tests/1,
	 statics_example_tests/1,
	 templates_example_tests/1,
	 cookies_example_tests/1
	]).

all() -> [
	  url_tests,
	  cookies_example_tests,
	  templates_example_tests,
	  statics_example_tests,
	  sessions_example_tests,
	  miscellaneous_example_tests,
	  upload_file_example_tests,
	  hello_world_example_tests,
	  escript_tests
	 ].

init_per_testcase(cookies_example_tests, Config) ->
    %% compile app
    {ok, ExampleDir} = compile_example_mods(cookies),   
   
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



init_per_testcase(url_tests, Config) ->
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
%% cookies_example_tests
%%======================================================

cookies_example_tests(_Config)->
    Cookie = "cookietest",
    Url = "http://127.0.0.1:8000",
    {200, _Body1, SessionKey} = request(Url),
    true = check_new_cookie(Url ++ "/?name=" ++ Cookie ++ "&value=12345", Cookie, SessionKey),
    false = check_new_cookie(Url, Cookie, SessionKey),
    ok.

check_new_cookie(Url, NewCookie, SessionKey)->
    Headers = 
	case SessionKey of
	    Undefined when Undefined == undefined ->
		[];
	    Session ->
		
		[{"Cookie", "msession=" ++ Session}]
	end,
    Request = {Url, Headers},
    {ok, {{_, _Code,_}, ReceivedHeaders, _Body}}= httpc:request(get, Request, [{timeout, 3000}], []),
    case proplists:get_value("set-cookie", ReceivedHeaders) of
	undefined -> false;
	Val ->
	    lists:any(
	      fun(E) ->
		      Splited =  string:tokens(Val, "="),
		      lists:member(NewCookie, Splited)
	      end,
	      string:tokens(Val, "; "))
    end.





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
    {404, _Body1, _} = request(Url ++ "/undefined"),
    error_logger:info_msg("code 404 -> ok~n"),
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
    %% build urls
    "/home" = minino_api:build_url(home_page2, []),
    "/test/testword"= minino_api:build_url(test_page, ["testword"]),
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
%% Url Tests
%%======================================================
url_tests(_Config)->
    Rules = get_dispatch_rules(),

    %% match urls

    {match, 
     home_page,
     home_view,
     []} = minino_dispatcher:match_url("/", Rules),

    {match, 
     home_page2,
     home_view,
     []} = minino_dispatcher:match_url("/home/", Rules),

    {match,
     test_page,
     test_view,
     ["arg1"]} = minino_dispatcher:match_url("/test/arg1/", Rules),

    {match, 
     articles_page,
     articles_view,
     ["2013"]} = minino_dispatcher:match_url("/articles/2013/", Rules),

    {match, 
     article_page,
     article_view,
     ["2013", "05"]} = minino_dispatcher:match_url("/articles/2013/05/", Rules),

    {match,
     get_page,
     get_view,
     ["03","json"]} = minino_dispatcher:match_url("/get/03/blog.json/", Rules),

    {match,
     get_page,
     get_view,
     ["03","xml"]} = minino_dispatcher:match_url("/get/03/blog.xml/", Rules),

    {match,
     posts_page,
     posts_view,
     ["03/blog.json/"]} =
	minino_dispatcher:match_url("/post/03/blog.json/", Rules),

    nomatch =minino_dispatcher:match_url("/nodefined/", Rules),

    %% Build Urls

    "/" = 
    	minino_dispatcher:build_url(home_page, 
				    [], 
				    Rules),

    "/home/" = 
    	minino_dispatcher:build_url(home_page2, 
				    [], 
				    Rules),


    "/articles/2013/" = 
    	minino_dispatcher:build_url(articles_page, 
				    ["2013"], 
				    Rules),

    "/articles/2013/05/" = 
    	minino_dispatcher:build_url(article_page, 
				    ["2013", "05"], 
				    Rules),
    "/get/03/blog.json/" =
  	minino_dispatcher:build_url(get_page, 
				    ["03", "json"], 
				    Rules),

    "/get/03/blog.xml/" =
  	minino_dispatcher:build_url(get_page, 
				    ["03", "xml"], 
				    Rules),


    ok.

get_dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
     {home_page, "^/$", home_view},
     {home_page2, "^/home/$", home_view},
     {test_page, "^/test/(\\w*)/$", test_view},
     {articles_page, "^/articles/(\\d{4})/$", articles_view},
     {article_page, "^/articles/(\\d{4})/(\\d{2})/$", article_view},
     {get_page, "^/get/(\\d{2})/blog.(\\w{3,4})/$", get_view},
     {posts_page, "^/post/(.*)", posts_view}
    ].


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
