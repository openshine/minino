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

-define(MSESSION, <<"msession">>).

-define(DEFAULTSESSIONTIME, 604800).

-type template_path() :: string().
-type template_args() :: [{atom(), string()}].
-type minino_req() :: term().
