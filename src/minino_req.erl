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

response({error, Code}, MReq) ->
    ErrorMsg = create_msg_error(Code),
    {ErrorMsg, Code, MReq};

response(Msg, MReq) ->
     {list_to_binary(Msg), 200, MReq}.

path(MReq) ->
    {BinPath, _} = cowboy_req:path(MReq#mreq.creq),
    string:tokens(binary_to_list(BinPath), "/").

create_msg_error(404) ->
    <<"Error 404: Not found">>;

create_msg_error(Code) when is_integer(Code) ->
    Msg = lists:flaten(io_lib:format("error ~p", [Code])),
    list_to_binary(Msg).
