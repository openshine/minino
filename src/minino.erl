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

-module(minino).


%%API
-export([main/1,
	 start/0,
	 stop/0]).

%% escript
main(Args) ->
    minino_escript:main(Args).

start()->
    ok = ensure_start(crypto),
    ok = ensure_start(ranch),
    ok = ensure_start(cowboy),
    ok = ensure_start(minino).  
					   	   
stop()->
    application:stop(crypto),
    application:stop(ranch),
    application:stop(cowboy),
    application:stop(minino),
    try 
	minino_shell ! stop
    catch _:_ -> 
	    ignore
    end.


ensure_start(App) ->
    case  application:start(App) of
 	ok-> ok;
 	{error,{already_started,_}} -> ok;
 	Error ->
	    error_logger:error_msg("start app ~p: ~p", [App, Error]),
	    Error
     end.
