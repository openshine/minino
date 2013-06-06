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
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %% read mapp conf
    {ok, MConf} = minino_config:read_file(), 
    
    %% change node name
    Node = proplists:get_value(node_name, MConf, 'minino'),
    net_kernel:start([Node, shortnames]),
    error_logger:info_msg("Node: ~p~n", [node()]),

    %% set cookie
    Cookie = proplists:get_value(cookie, MConf, 'minino_default_cookie'),
    erlang:set_cookie(node(), Cookie),
    error_logger:info_msg("cookie: ~p~n", [erlang:get_cookie()]),

    Port = proplists:get_value(port, MConf, 8080),
    ConfMediaUrl = proplists:get_value(media_url, MConf, ["media"]),
    MediaUrl = "/" ++ string:join(ConfMediaUrl, "/") ++ "/[...]",
    MediaPath = proplists:get_value(media_path, MConf, "priv/static"),
    error_logger:info_msg("media url: ~p~n", [MediaUrl]),
    error_logger:info_msg("static dir: ~p~n", [MediaPath]),
    %% start cowboy
    Dispatch = cowboy_router:compile([
    				      {'_', [
    					     {MediaUrl, 
					      cowboy_static, 
					      [{directory, MediaPath}, 
					       {mimetypes, 
						{fun mimetypes:path_to_mimes/2, default}}
					      ]},
					     {'_', 
					      minino_cowboy_handler, 
					      []}
					    ]}
    				     ]),
    {ok, _} = 
    	cowboy:start_http(http, 
    			  100,
    			  [{port, Port}], 
    			  [{env, [{dispatch, Dispatch}]}]
    			 ), 
    error_logger:info_msg("~nstart minino web server: http://127.0.0.1:~p~n", [Port]),
    Params = [MConf],
    minino_sup:start_link(Params).

stop(_State) ->
    ok.

