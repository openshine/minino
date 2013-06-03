%%minino application

-module(statics).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3]).

%% minino funs

init(_MConf) ->
    {ok, []}.

dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view}
    ].


%% views

home_view(MReq, _Args, _Term) ->
    {ok, Html} = minino_api:render_template("home.html", [{text, "Meow!!"}]),
    minino_api:response(Html, MReq).
