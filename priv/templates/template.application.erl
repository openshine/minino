%%minino application

-module({{ application }}).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3,
	 test_view/3]).

%% minino funs

init(_MConf) ->
    {ok, []}.

dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
     {home_page, "^/$", home_view},
     {home_page2, "^/home$", home_view},
     {test_page, "^/test/(\\w*)$", test_view}
    ]. 



%% views

home_view(MReq, _Args, _Term) ->
    {ok, Html} = minino_api:render_template("home.html", [{text, "Meow!!"}]),
    minino_api:response(Html, MReq).

test_view(MReq, [Val], _Term) ->
    Html = lists:flatten(io_lib:format("<html><body>test: ~s</body></html>", [Val])),
    minino_api:response(Html, MReq).
