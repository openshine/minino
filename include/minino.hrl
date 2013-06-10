-type template_path() :: string().
-type template_args() :: [{atom(), string()}].
-type minino_req() :: hidden().
-type minino_conf() :: [{term(),term()}].
-type hidden() :: term().


-record(mreq,  {creq,
		build_url_fun,
		session,
		from}).

-define(DEFAULTSESSIONTIME, 604800).
-define(DEFAULTSESSIONNAME, "msession").


-define(DBG(Str, P), io:format(Str, P)).
-define(DBG(Str), io:format(Str)).
