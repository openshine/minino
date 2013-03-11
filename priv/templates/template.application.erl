%%minino application

-module({{ application }}).

%% minino funs
-export([dispatch_rules/0]).


%% views
-export([home_view/2]).

dispatch_rules() ->
    [		
		%% {Id::atom(), Path::[string()|atom], view::atom()}
		{home_page, [], home_view}
    ].


home_view([], _Req) ->
    minino:response("<html><body>hello world!</body></html>").
