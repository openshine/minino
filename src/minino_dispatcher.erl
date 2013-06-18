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
%%% Created :  13 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_dispatcher).

-behaviour(gen_server).

-include("include/minino.hrl").

%% API
-export([start_link/1,
	 dispatch/1,
	 response/3,
	 update_rules/0,
	 match_url/2,
	 build_url/2,
	 build_url/3
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {mapp,
		mconf,
		app_term,
		dispatch_rules
	       }).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Params::[term()]) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).


dispatch(MReq) ->
    Ref = make_ref(),
    gen_server:cast(?SERVER, {dispatch, MReq, self(), Ref}),
    Ref.

response(To, MResponse, Ref) ->
    To ! {response, Ref, MResponse}.

update_rules() ->
    gen_server:call(?SERVER, update_rules).


build_url(Id, Args) ->
    gen_server:call(?SERVER, {build_url, Id, Args}).
			    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([MConf]) ->
    MApp = proplists:get_value(app_mod, MConf),
    {ok, AppTerm} =  MApp:init(MConf),
    {ok, #state{mapp=MApp,
    		mconf=MConf,
    		app_term=AppTerm,
    		dispatch_rules=MApp:dispatch_rules()
    	       }}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(update_rules, _From, State) ->
    MApp =State#state.mapp,
    {ok, AppTerm} =  MApp:init(State#state.mconf),
    {reply, ok, State#state{
			    app_term=AppTerm,
			    dispatch_rules=MApp:dispatch_rules()
			   }};

handle_call({build_url, Id, Args}, _From, State) ->
    Reply = build_url(Id, Args, State#state.dispatch_rules),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({dispatch, MReq, From, Ref}, State) ->
    Path = minino_api:path(MReq),
    MReq1 = MReq#mreq{from=From},
    case  match_url(Path, State#state.dispatch_rules) of
	nomatch ->
	    Response = minino_api:response({error, 404}, MReq1),
	    minino_dispatcher:response(From, Response, Ref);
	{match, _Id, View, Args}  ->

	    %% create worker process
	    WSpec = {make_ref(), 
	    	     {minino_req_worker, start_link, []}, 
	    	     permanent, 
	    	     5000, 
	    	     worker, 
	    	     dynamic},
	    {ok, Pid} = supervisor:start_child(minino_dispatcher_sup, WSpec),
	    Params = [MReq1, 
		      View,
		      Args,
		      State#state.mapp, 
		      State#state.app_term,
		      Ref],
		gen_server:cast(Pid, {work, Params})

    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

    
match_url(Url, DispatchRules) ->
    match_url_loop(DispatchRules, Url).

match_url_loop([], _Url)->
    nomatch;

match_url_loop([{Id, RegexUrl, View}=_Rule|Rest], Url)->
    case match_sinlge_url(Url, RegexUrl) of
	{match, Args} ->
	    {match, Id, View, Args};
	nomatch ->
	    match_url_loop(Rest, Url)
    end.

    
match_sinlge_url(Url, RegexUrl) ->
    re:run(Url, RegexUrl, [{capture, all_but_first, list}]).

build_url(Id, Args, DispatchRules) ->
    case lists:keyfind(Id, 1, DispatchRules) of
	false -> {error, "id not found"};
	{Id, RegexUrl, _View} ->
	    inverse_url(RegexUrl, Args)
    end.


inverse_url(RegexUrl, Args)->
    List = string:tokens(RegexUrl, "()"),
    inverse_url_loop(List, Args, []). 

inverse_url_loop([], _Args, Acc) ->
    inverse_url_end(lists:reverse(Acc));
inverse_url_loop(Rest, [], Acc) ->
    inverse_url_end(lists:reverse(Acc) ++ Rest);

inverse_url_loop([E|Rest], [Arg|RestArgs]=AllArgs, Acc) ->
    case re:run(Arg, E, [{capture, 
			  all_but_first, 
			  list}]) of
	nomatch ->
	    inverse_url_loop(Rest, AllArgs, [E|Acc]);
	{match, _} ->
	    inverse_url_loop(Rest, RestArgs, [Arg|Acc])

    end.
    


inverse_url_end(List) ->
    L = lists:flatten(List),
    L1 =
	case L of
	    [$^|R] -> R;	 
	    R -> R
	end,
    case lists:reverse(L1) of
	[$$|R1] ->  lists:reverse(R1);	 
	_ -> L1
    end.


