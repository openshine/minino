%%minino application

-module(kitty).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/2,
	 test_view/2,
	 upload_view/2,
	 check_module/0]).


%% minino funs

init(_MConf) ->
    ok.

dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view},
     {home_page, ["home"], home_view},
     {test_page, ["test", testvalue], test_view},
     {upload_page, ["upload"], upload_view}
    ].


%% views

home_view(MReq, _Args) ->
    {ok, Html} = minino_api:render_template("home.html", [{text, "Meow!!"}]),
    io:format("dbg: method: ~p~n", [minino_api:get_method(MReq)]),
    minino_api:response(Html, MReq).

test_view(MReq, Args) ->
    TestVal = proplists:get_value(testvalue, Args),
    Html = lists:flatten(io_lib:format("<html><body>test: ~s</body></html>", [TestVal])),
    minino_api:response(Html, MReq).

upload_view(MReq, _Args) ->
    {ok, Html} = minino_api:render_template("uploadfile.html", []),
    minino_api:response(Html, MReq).


check_module()->
    ?MODULE.
