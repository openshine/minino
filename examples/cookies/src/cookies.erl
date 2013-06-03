%%minino application

-module(cookies).

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
     {root_page, [], home_view},
     {home_page, ["home"], home_view}
    ].


%% views
home_view(MReq, _Args, _State) ->
    ReqParams = minino_api:url_params(MReq), 
    Name = proplists:get_value(<<"name">>, ReqParams),
    Value = proplists:get_value(<<"value">>, ReqParams),
    {MReq1, CurrentCookie} =
	case {Name, Value} of
	    {Name, Value} when Name /= undefined, Value /= undefine ->
		NewMReq =  minino_api:set_cookie(MReq, Name, Value),
		NewCookie = [{binary_to_list(Name), 
			      binary_to_list(Value)}],
		{NewMReq, NewCookie};
	    _Else ->
		{MReq, []}
	end,
    Cookies = minino_api:get_cookies(MReq) ++ CurrentCookie,
    {ok, Html} = minino_api:render_template("form.html", [{cookies, Cookies}]),
    minino_api:response(Html, MReq1).
