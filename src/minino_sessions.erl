%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  10 Apr 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_sessions).
-include("include/minino.hrl").

-export([get_or_create/1]).



%% @doc get or create session.
-spec get_or_create(MReq::term()) -> {ok, Session::string(), MReq1::term()} | {error, Reason::term()}.
get_or_create(MReq) ->
    CReq = MReq#mreq.creq,
    {StoredSession, CReq1} = cowboy_req:cookie(?MSESSION, CReq),
    {Session, CReq2} = 
    	case StoredSession of
    	    undefined ->
    		NewSession = random_md5(),
    		R = cowboy_req:set_resp_cookie(
    		      ?MSESSION, 
    		      NewSession, [], CReq1),
    		{NewSession, R};	  
    	    S ->
    		%% io:format("dbg s: ~p~n", [S]),
    		{S, CReq1}
    	end,
    {ok, Session, MReq#mreq{creq=CReq2}}.


random_md5() ->
    Str = lists:flatten(io_lib:format("~p", [now()])),
    Md5 = erlang:md5(Str),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Md5]).
  
