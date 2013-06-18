%%minino application

-module(upload_file).

%% minino funs
-export([init/1,
	 dispatch_rules/0]).


%% views
-export([upload_view/3]).

-record(term, {file_name}).

%% minino funs

init(MConf) ->
    FileName = proplists:get_value(file_name, MConf),
    filelib:ensure_dir(FileName),
    {ok, #term{file_name=FileName}}.

dispatch_rules() ->
    [%% {Id::atom(), RegexUrlPath::string(), view::atom()}
        {home_page, "^/upload$", upload_view}
    ].


%% views
upload_view(MReq, _Args, Term) ->
    case minino_api:get_method(MReq) of
	"GET" -> 
	    {ok, Html} = minino_api:render_template("uploadfile.html", []),
	    minino_api:response(Html, MReq);
	"POST" ->
	    ok = minino_api:get_file(MReq, Term#term.file_name),
	    minino_api:response("ok", MReq)
    end.
