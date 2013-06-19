%%minino application

-module(sessions).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([home_view/3]).

-define(KEY, "key").

%% minino funs

init(_MConf) ->
    {ok, []}.

dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
        {home_page, "^/$", home_view}
    ].


%% views
home_view(MReq, _Args, _Term) ->
    ReqParams = minino_api:url_params(MReq), 
    Dict = minino_api:get_session_dict(MReq),
    Value = 
	case proplists:get_value(<<"value">>, ReqParams) of
	    undefined ->
		case dict:find(?KEY, Dict) of
		    error ->
			"undefined";
		    {ok, Val} ->
    			Val
		end;
	    BinParam ->
		Param = binary_to_list(BinParam),
		NewDict = dict:store(?KEY, Param, Dict),
		{ok, _} = minino_api:update_session_dict(MReq, NewDict),
		Param
	end,
    {ok, Html} = minino_api:render_template("form.html", [{value, Value}]),
    minino_api:response(Html, MReq).
