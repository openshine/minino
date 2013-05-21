-record(mreq,  {creq,
		build_url_fun,
		mconf,
		file,
		session,
		from}).

-define(DEFAULTSESSIONTIME, 604800).
-define(DEFAULTSESSIONNAME, "msession").


-define(DBG(Str, P), io:format(Str, P)).
-define(DBG(Str), io:format(Str)).
