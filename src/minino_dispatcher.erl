%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  13 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_dispatcher).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 dispatch/1,
	 response/2,
	 update_rules/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {mapp,
		mconf,
		match_fun}).


-compile([export_all]).

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


dispatch(Req) ->
    gen_server:call(?SERVER, {dispatch, Req}).


response(To, MResponse) ->
    gen_server:reply(To, MResponse).


update_rules() ->
    gen_server:call(?SERVER, update_rules).
    

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
    MatchFun = create_match_fun(MApp:dispatch_rules()),
    {ok, #state{mapp=MApp,
		mconf=MConf,
		match_fun=MatchFun
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
handle_call({dispatch, Req}, From, State) ->
    %% create worker process
    Params = [Req, 
	      State#state.mapp, 
	      State#state.match_fun,  
	      State#state.mconf, 
	      From],
    WSpec = {make_ref(), 
	     {minino_req_worker, start_link, [Params]}, 
	     permanent, 
	     5000, 
	     worker, 
	     dynamic},
    {_, {normal_stop, _}} = supervisor:start_child(minino_dispatcher_sup, WSpec),
    {noreply, State};

handle_call(update_rules, _From, State) ->
     MatchFun = create_match_fun(MApp:dispatch_rules()),
    {reply, ok, State#state{match_fun=MatchFun);

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


%% @spec create_disp_fun(DispRules) -> F
%% DispRules = [rule()]
%% rule() =  {Id::atom(), Path::[string()|atom()], view::atom()}, 
%% F::fun() 
%% @end
create_match_fun(DispRules)->
    ParsedRules = [parse_rule(Rule) || Rule <- DispRules],
    Clauses =
	lists:foldl(
	  fun({{M, R, _C}, View}, Acc) ->
		  Clause = lists:flatten(io_lib:format("(~s) -> {~p, ~s}", [M, View, R])), 
		  Acc ++ Clause ++ ";\n"
	  end,
	  [],
	  ParsedRules),
    FStr =  "fun"  ++ Clauses ++ "(_Esle) -> undefined \nend.",
    {ok,Scanned,_} = erl_scan:string(FStr),
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, F, _} = erl_eval:exprs(Parsed,[]),
    F.		  

parse_rule({_Id, [], View}) ->
    {{"[]","[]", 0}, View};

parse_rule({_Id, RulePath, View}) ->
    {create_match_str(RulePath), View}.

	    
create_match_str(RulePath) ->
     create_match_str(RulePath, []).

create_match_str([], Acc) ->
    close_match(Acc);

create_match_str(['*'|_], Acc) ->
    append_match({tail}, Acc);

create_match_str([Var|R], Acc) when is_atom(Var) ->
    create_match_str(R, append_match(Var, Acc));

create_match_str([String|R], Acc) ->
    create_match_str(R, append_match(String, Acc)).


append_match(E, [])->
    Acc = {"", "", 0},
    append_match(E, Acc);
    
append_match({tail}, {M, R, C}) ->
    case M of
	"" -> {"V", "V", 1};
	M -> 
	    V = lists:flatten(io_lib:format("V~p", [C])),
	    M1 = M ++ " | " ++ V,
	    R1 = 
		case R of
		    "" -> V;
		    R -> R ++ ", " ++ V
		end,
	    close_match({M1, R1, C+1})
    end;

append_match(Var, {M, R, C}) when is_atom(Var) ->
    V = lists:flatten(io_lib:format("V~p", [C])),
    case M of
	"" -> {V, V, C+1};
	M -> 
	    M1 = M ++ ", " ++ V,
	    R1 = case R of
		     "" -> V; 
		     R -> R ++ ", " ++ V
		 end,
	    {M1, R1, C+1}
    end;

append_match(Str, {M, R, C}) when is_list(Str) ->
    case M of
	"" -> {escp(Str), R, C};
	M ->
	    M1 = M ++ ", " ++ escp(Str),
	    {M1, R, C}
    end.
    
close_match({M, R, C})->
    M1 = case M of
	     [] -> "[]";
	     M -> "[" ++ M ++ "]"
	 end,
    R1 = case R of
	     [] -> "[]";
	     R -> "[" ++ R ++ "]"
	 end,
    
    {M1, R1, C}.

escp(S) -> "\"" ++ S ++ "\"".
    

