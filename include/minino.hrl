%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created : 3 Apr 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-record(mreq,  {creq,
		build_url_fun,
		mconf,
		file,
		session}).

-define(DEFAULTSESSIONTIME, 604800).
-define(DEFAULTSESSIONNAME, "msession").

-type template_path() :: string().
-type template_args() :: [{atom(), string()}].
-type minino_req() :: term().



-define(DBG(Str, P), io:format(Str, P)).
-define(DBG(Str), io:format(Str)).
