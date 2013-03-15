%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  15 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_api).


-export([response/2,
	 path/1]).


response({error, 404}, Req) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"Error 404: Not found">>, Req),
    Req2;

response(Msg, Req) when is_list(Msg) ->
    {ok, Req2} = cowboy_req:reply(200, 
				  [], 
				  list_to_binary(Msg), 
				  Req),
    Req2.


path(Req) ->
    {BinPath, _} = cowboy_req:path(Req),
    string:tokens(binary_to_list(BinPath), "/").
	
