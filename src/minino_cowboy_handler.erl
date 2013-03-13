%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  12 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_cowboy_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, CReq, []) ->
    {ok, CReq, undefined}.

handle(CReq, State) ->
    MReq = req_cowboy_to_minino(CReq),
    {ok, CReq2} = cowboy_req:reply(200, [], <<"Hello world!">>, CReq),
    {ok, CReq2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

req_cowboy_to_minino(CReq) ->
    CReq.
