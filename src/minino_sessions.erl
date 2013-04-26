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
	 set_cookie/3,
	 get_dict/1,
	 update_dict/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(DB, ?MODULE).

-define(PURGETIME, 60). %% db purge time in seconds.

-record(state, {session_time,
		secret_key,
		session_name,
		session_cookie_httponly,
		session_cookie_domain,
		session_cookie_path,
		session_cookie_secure
	       }).

%% -record(session, {key, 
%% 		  dict, 
%% 		  new=true, 
%% 		  modified=true, 
%% 		  time}).




-record(mreq_session, {new, key, modified}).
-record(stored_session, {key, dict, time}).



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
    gen_server:call(?SERVER, {get_or_create_session, MReq}).



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
set_cookie(MReq, CookieName, CookieVal) ->
    CReq = MReq#mreq.creq,
    CReq1 = cowboy_req:set_resp_cookie(
	      CookieName, 
	      CookieVal, [], CReq),
    MReq#mreq{creq=CReq1}.




%% @doc get minino session dict.
-spec get_dict(MReq::minino_req()) -> Dict::dict().
get_dict(MReq) ->
    gen_server:call(?SERVER, {get_dict, MReq}).

%% @doc update minino session dict.
-spec update_dict(MReq::minino_req(), Dict::dict()) -> 
				 {ok, MReq1::minino_req()} | {error, Error::term()}.
update_dict(MReq, Dict) ->
    gen_server:call(?SERVER, {update_dict, MReq, Dict}).
    


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
    SessionTime = proplists:get_value(session_time, Mconf, ?DEFAULTSESSIONTIME),
    SecretKey = 
    	case proplists:get_value(secret_key, Mconf, error) of
    	    S when S /= error ->
    		S
    	end,
    SessionName = list_to_binary(
		    proplists:get_value(session_cookie_name, 
					Mconf, 
					?DEFAULTSESSIONNAME)), 
    HttpOnly = proplists:get_value(session_cookie_httponly, Mconf, true),
    SessionDomain =
	case proplists:get_value(session_cookie_domain, Mconf, none) of
	    SDomain when is_list(SDomain) ->
		list_to_binary(SDomain);
	    SDomain -> SDomain
	end,
    SessionPath =
	case proplists:get_value(session_cookie_path, Mconf, none) of
	    SPath when is_list(SPath) ->
		list_to_binary(SPath);
	    SPath -> SPath
	end,
    SessionSecure =  proplists:get_value(session_cookie_secure, Mconf, false),
    ask_purge_db(?PURGETIME),
    ets:new(?DB, [set, 
		  named_table,
		  {keypos, #stored_session.key}]),
    {ok, #state{session_time=SessionTime,
		secret_key=SecretKey,
		session_name=SessionName,
		session_cookie_httponly=HttpOnly,
		session_cookie_domain=SessionDomain,
		session_cookie_path=SessionPath,
		session_cookie_secure=SessionSecure
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
handle_call({get_dict, MReq}, _From, State) ->
    Reply =  get_dict_db(MReq),
    {reply, Reply, State};

handle_call({update_dict, MReq, Dict}, _From, State) ->
    Reply =  update_dict_db(MReq, Dict),
    {reply, Reply, State};

handle_call({get_or_create_session, MReq}, _From, State) ->
    Reply =  get_or_create_session(MReq, State),
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
    ask_purge_db(?PURGETIME),
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

get_or_create_session(MReq, State) ->
    CReq = MReq#mreq.creq,
    {BrowserSessionKey, CReq1} = cowboy_req:cookie(State#state.session_name, CReq),
    {MreqSession, CReq2} =
	case cowboy_req:path(CReq) of
	    {<<"/favicon.ico">>, _} -> 
		{#mreq_session{key=undefined, new=false}, CReq1};
	    {_Path, _Creq} ->
		StoredSes = get_session(BrowserSessionKey),
		case StoredSes of
		    undefined ->
			NewStoredSession = create_session(State),
			CowBoyCookieOps = create_cowboy_cookie_ops(State),
			Key = NewStoredSession#stored_session.key,
			NewCReq = 
			    cowboy_req:set_resp_cookie(
			      State#state.session_name, 
			      Key, 
			      CowBoyCookieOps, 
			      CReq1),
			MReqSes = 
			    #mreq_session{key=Key, 
					  new=true,
					  modified=true
					 },
			{MReqSes, NewCReq};
		    StoredSes ->
			Key = StoredSes#stored_session.key,
			MReqSes = 	 
			    #mreq_session{key=Key, 
					  new=false,
					  modified=false},
			
			{MReqSes, CReq1}
		end
	end,
    MReq1 = MReq#mreq{creq=CReq2,
		     session=MreqSession},
    {ok, MReq1}.

create_key(SecrectKey) ->
    Str = lists:flatten(io_lib:format("~p", [now()])),
    Md5 = erlang:md5(Str ++ SecrectKey),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Md5]).



get_session(undefined) ->
    undefined;

get_session(SessionKey) when is_binary(SessionKey) ->
    get_session(binary_to_list(SessionKey));

get_session(SessionKey) ->
    case ets:lookup(?DB, SessionKey) of
	[] -> undefined;
	[StoredSession] ->
	    Now = now_secs(),
	    case StoredSession#stored_session.time of
		Time when Time < Now -> 
		    Key = StoredSession#stored_session.key,
		    ets:delete(?DB, Key),
		    undefined;
		_E ->
		    StoredSession
	    end
    end.


    
create_session(State) ->
    SessionTime = State#state.session_time,
    Key = create_key(State#state.secret_key),
    Dict = dict:new(),
    Time = create_expired_time(SessionTime),
    StoredSession = #stored_session{key=Key, 
		       dict=Dict, 
		       time=Time},
    true = ets:insert(?DB, StoredSession),
    StoredSession.


create_expired_time(SessionTime) ->
    now_secs() + SessionTime.

now_secs() ->
    {Mega, Secs, _Mili} = now(),
    Mega*1000000 + Secs.

purge_db() ->
    Now =  now_secs(),
    Tuple = #stored_session{time='$1', _='_'},
    MatchSpec = [{Tuple,[{'=<','$1', Now}],[true]}],
    ets:select_delete(?DB, MatchSpec).

create_cowboy_cookie_ops(State)->
    HttpOnly = State#state.session_cookie_httponly,
    Ops = 
	[
	 {http_only, HttpOnly}
	],

    Ops1 = 
	case State#state.session_cookie_domain of
	    none -> Ops;
	    SessionDomain ->
		[{domain, SessionDomain}|Ops]
	end,
    Ops2 = case State#state.session_cookie_path of
	       none -> Ops1;
	       SessionPath ->
		   [{path, SessionPath}|Ops1]
	   end,
    case State#state.session_cookie_secure of
	true ->
	    [{secure, true}|Ops2];
	_Else -> Ops2
    end.



update_dict_db(MReq, Dict)->
    MReqSession = MReq#mreq.session,
    Key = MReqSession#mreq_session.key,
    case ets:lookup(?DB, Key) of
	[] -> {error, "session not found"};
	[StoredSes] -> 
	    NewStoredSession = StoredSes#stored_session{dict=Dict},
	    true = ets:insert(?DB, NewStoredSession),
	    NewMReqSes = MReqSession#mreq_session{new=false, modified=true},
	    {ok, NewMReqSes}
    end.



get_dict_db(MReq) ->
    MReqSession = MReq#mreq.session,
    Key = MReqSession#mreq_session.key,
    case ets:lookup(?DB, Key) of
	[] -> {error, "session not found"};
	[S] -> S#stored_session.dict
    end.


ask_purge_db(TimeSecs) ->
  erlang:send_after(TimeSecs*1000, self(), purge_db).
