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

-behaviour(gen_server).



%% API
-export([start_link/1]).
-export([get_or_create/1,
	 get_cookie/2,
	 set_cookie/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(DB, ?MODULE).

-define(PURGETIME, 60). %% db purge time in seconds.

-record(state, {sessiontime}).

-record(session, {key, 
		  dict, 
		  new=true, 
		  modified=true, 
		  permanent=false, 
		  time}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Params, []).


%% @doc get or create session.
-spec get_or_create(MReq::term()) -> {ok, Session::string(), MReq1::term()} | {error, Reason::term()}.
get_or_create(MReq) ->
    gen_server:call(?SERVER, {get_or_create, MReq}).



%% @doc get cookie.
-spec get_cookie(MReq::minino_req(), CookieName::string()) -> string()|undefined.
get_cookie(MReq, CookieName) ->
    CReq = MReq#mreq.creq,
    CookieNameBin = list_to_binary(CookieName),
    case cowboy_req:cookie(CookieNameBin, CReq) of
    	{undefined, _CReq} -> undefined;
    	{Cookie, _CReq} -> binary_to_list(Cookie)
    end.
    

%% @doc set cookie.
-spec set_cookie(MReq::minino_req(), CookieName::string(), CookieVal::string()) -> 
			MReq1::minino_req() | {error, term()}.
set_cookie(_MReq, ?MSESSION, _CookieVal) ->
    {error, "reserved cookie name"};

set_cookie(MReq, CookieName, CookieVal) ->
    CReq = MReq#mreq.creq,
    CReq1 = cowboy_req:set_resp_cookie(
	      CookieName, 
	      CookieVal, [], CReq),
    MReq#mreq{creq=CReq1}.


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
init([Mconf]) ->
    SessionTime = proplists:get_value(sessiontime, Mconf, ?DEFAULTSESSIONTIME),
    ets:new(?DB, [set, 
		  named_table,
		  {keypos, #session.key}]),
    erlang:send_after(?PURGETIME*1000, self(), purge_db),
    {ok, #state{sessiontime=SessionTime}}.

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
handle_call({get_or_create, MReq}, _From, State) ->
    CReq = MReq#mreq.creq,
    {BrowserSessionKey, CReq1} = cowboy_req:cookie(?MSESSION, CReq),
    {Session, CReq2} =
	case cowboy_req:path(CReq) of
	    {<<"/favicon.ico">>, _} -> 
		{#session{key=undefined, new=false}, CReq1};
	    _Else ->
		S = get_session(BrowserSessionKey, State#state.sessiontime),
		io:format("session: ~p~n", [S]),
		case S#session.new of
		    true ->
			NewCReq = 
			    cowboy_req:set_resp_cookie(
			      ?MSESSION, 
			      S#session.key, [], CReq1),
			{S, NewCReq};
		    false ->
			{S, CReq1}
		end
	end,
    Reply =  {ok, Session, MReq#mreq{creq=CReq2}},
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
handle_info(purge_db, State) ->
    purge_db(),
    erlang:send_after(?PURGETIME*1000, self(), purge_db),
    {noreply, State};

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



random_md5() ->
    Str = lists:flatten(io_lib:format("~p", [now()])),
    Md5 = erlang:md5(Str),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Md5]).



get_session(undefined, SessionTime) ->
    create_session(SessionTime);

get_session(SessionKey, SessionTime) when is_binary(SessionKey) ->
    get_session(binary_to_list(SessionKey), SessionTime);

get_session(SessionKey, SessionTime) ->
    %%check stored session
    case ets:lookup(?DB, SessionKey) of
	[] -> create_session(SessionTime);
	[Session] ->
	    Now = now_secs(),
	    case Session#session.time of
		Time when Time < Now -> 
		    ets:delete(?DB, Session),
		    create_session(SessionTime);
		_E ->
		    Session#session{new=false}
	    end
    end.


    
create_session(SessionTime) ->
    Key = random_md5(),
    Dict = dict:new(),
    Time = create_expired_time(SessionTime),
    Session = #session{key=Key, dict=Dict, time=Time},
    true = ets:insert(?DB, Session),
    Session.


create_expired_time(SessionTime) ->
    now_secs() + SessionTime.



now_secs() ->
    {Mega, Secs, _Mili} = now(),
    Mega*1000000 + Secs.


purge_db() ->
    Now =  now_secs(),
    Tuple = #session{time='$1', _='_'},
    MatchSpec = [{Tuple,[{'=<','$1', Now}],[true]}],
    ets:select_delete(?DB, MatchSpec).
