%%%-------------------------------------------------------------------
%%% Copyright (c) Openshine s.l.  and individual contributors.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without modification,
%%% are permitted provided that the following conditions are met:
%%% 
%%%     1. Redistributions of source code must retain the above copyright notice, 
%%%        this list of conditions and the following disclaimer.
%%%     
%%%     2. Redistributions in binary form must reproduce the above copyright 
%%%        notice, this list of conditions and the following disclaimer in the
%%%        documentation and/or other materials provided with the distribution.
%%% 
%%%     3. Neither the name of Minino nor the names of its contributors may be used
%%%        to endorse or promote products derived from this software without
%%%        specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
%%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
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
	 path/1,
	 get_method/1,
	 get_params/1
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
    <<"Error 404: Not found.">>;
create_msg_error(500) ->
    <<"Error 500: Internal server error.">>;
create_msg_error(Code) when is_integer(Code) ->
    Msg = lists:flaten(io_lib:format("error ~p", [Code])),
    list_to_binary(Msg).


%% @doc get method
-spec get_method(MReq::minino_req()) -> string().
get_method(MReq) ->
    {MethodBin,_Req} = cowboy_req:method(MReq#mreq.creq),
    binary_to_list(MethodBin).
    

%% @doc get request args
-spec get_params(MReq::minino_req()) -> [{Key::binary(), Value::binary()}].
get_params(MReq) ->
    {Args, _Creq} = cowboy_req:qs_vals(MReq#mreq.creq),
    Args.
