%%minino application

-module(hello_world).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3]).

%% minino funs

init(_MConf) ->
    {ok, []}.



dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
        {home_page, "^/$", home_view}
    ].

%% views
home_view(MReq, _Args, _Term) ->
    minino_api:response("Hello world!!", MReq).
