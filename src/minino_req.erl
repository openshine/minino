%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created : 3 Apr 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_req).


-include("include/minino.hrl").

-export([response/2,
	 path/1
	]). 


response({error, 404}, MReq) ->
    {ok, NewCReq} = cowboy_req:reply(404, [], <<"Error 404: Not found">>, MReq#mreq.creq),
    MReq#mreq{creq=NewCReq};

response(Msg, MReq) when is_list(Msg) ->
    {ok, NewCReq} = cowboy_req:reply(200, 
				     [], 
				     list_to_binary(Msg), 
				     MReq#mreq.creq),
    MReq#mreq{creq=NewCReq}.


path(MReq) ->
    {BinPath, _} = cowboy_req:path(MReq#mreq.creq),
    string:tokens(binary_to_list(BinPath), "/").
