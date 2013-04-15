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
-export([get_or_create/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(DB, ?MODULE).

-define(PURGETIME, 10). %% db purge time in seconds.

-record(state, {sessiontime}).



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
    ets:new(?DB, [set, named_table]),
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
    {StoredSession, CReq1} = cowboy_req:cookie(?MSESSION, CReq),
    Session =
	case  cowboy_req:path(CReq) of
	    {<<"/favicon.ico">>, _} -> 
		undefined;
	    _Else ->
		case StoredSession of
		    undefined ->
			not_valid;
		    SessionBin ->
			S = binary_to_list(SessionBin),
			case has_expired(S) of
			    true -> not_valid;
			    false -> S
			end
		end
	end,
    {Session1, CReq2} =
	case Session of
	    not_valid ->
		NewSession = add_new_session(State#state.sessiontime),
		R = cowboy_req:set_resp_cookie(
		      ?MSESSION, 
		      NewSession, [], CReq1),
		{NewSession, R};
	    Session -> {Session, CReq1}
	end,
    Reply =  {ok, Session1, MReq#mreq{creq=CReq2}},
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

add_new_session(SessionTime) ->
    Session = random_md5(),
    true = ets:insert(?DB, {Session, create_expired_time(SessionTime)}),
    Session.

create_expired_time(SessionTime) ->
    %% {Mega, Secs, _Mili} = now(),
    %% Timestamp = Mega*1000000 + Secs + SessionTime. 
    now_secs() + SessionTime.

has_expired(Session) ->
    Now = now_secs(),
    case ets:lookup(?DB, Session) of
	[] -> true;
	[{Session, Time}] when Time < Now -> 
	    ets:delete(?DB, Session),
	    true;
	_E -> 
	    false
    end.


now_secs() ->
    {Mega, Secs, _Mili} = now(),
    Mega*1000000 + Secs.


purge_db() ->
    Now =  now_secs(),
    MatchSpec = [{{'$1','$2'},[{'=<','$2', Now}],[true]}],
    ets:select_delete(?DB, MatchSpec).
