%%minino application

-module(hello_world).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3]).

-record(state, {}).

%% minino funs

init(_MConf) ->
    {ok, #state{}}.



dispatch_rules() ->
    [%% {Id::atom(), Path::[string()|atom()], view::atom()}
     {root_page, [], home_view}
    ].

%% views
home_view(MReq, _Args, _State) ->
    minino_api:response("Hello world!!", MReq).
