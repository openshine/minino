%%minino application

-module(statics).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3,
	 test_view/3,
	 upload_view/3]).

-record(state, {}).

%% minino funs

init(_MConf) ->
    {ok, #state{}}.

dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view},
     {home_page, ["home"], home_view},
     {test_page, ["test", testvalue], test_view},
     {upload_page, ["upload"], upload_view}
    ].


%% views

home_view(MReq, _Args, _State) ->
    {ok, Html} = minino_api:render_template("home.html", [{text, "Meow!!"}]),
    minino_api:response(Html, MReq).

test_view(MReq, Args, _State) ->
    TestVal = proplists:get_value(testvalue, Args),
    Html = lists:flatten(io_lib:format("<html><body>test: ~s</body></html>", [TestVal])),
    minino_api:response(Html, MReq).

upload_view(MReq, _Args, _State) ->
    {ok, Html} = minino_api:render_template("uploadfile.html", []),
    minino_api:response(Html, MReq).
