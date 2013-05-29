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
	 response/3,
	 update_rules/0,
	 create_match_fun/1,
	 create_build_url_fun/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {mapp,
		mconf,
		match_fun,
		build_url_fun,
		app_term
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
    BuildUrlFun = create_build_url_fun(MApp:dispatch_rules()),
    {ok, AppTerm} =  MApp:init(MConf),
    {ok, #state{mapp=MApp,
		mconf=MConf,
		build_url_fun=BuildUrlFun,
		match_fun=MatchFun,
		app_term=AppTerm
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
    Rules = MApp:dispatch_rules(),
    MatchFun = create_match_fun(Rules),
    {ok, AppTerm} =  MApp:init(State#state.mconf),
    {reply, ok, State#state{match_fun=MatchFun,
			   app_term=AppTerm}};

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
    %% create worker process
    WSpec = {make_ref(), 
    	     {minino_req_worker, start_link, []}, 
    	     permanent, 
	     5000, 
	     worker, 
	     dynamic},
    {ok, Pid} = supervisor:start_child(minino_dispatcher_sup, WSpec),
    Params = [MReq, 
    	      State#state.mapp, 
    	      State#state.match_fun,  
    	      State#state.build_url_fun,
    	      State#state.mconf, 
	      State#state.app_term,
    	      From, 
	      Ref],
    gen_server:cast(Pid, {work, Params}),
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


%% @spec create_match_fun(DispRules::[rule()]) -> F::fun()
%% DispRules = [rule()]
%% rule() =  {Id::atom(), Path::[string()|atom()], View::atom()}
%% 
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
    TupleStr = 
	"{" ++ 
	erlang:atom_to_list(Var) ++ 
	", " ++
	V ++ 
	"}",
    case M of
	"" -> {V, V, C+1};
	M -> 
	    M1 = M ++ ", " ++ V,
	    R1 = case R of
		     "" -> TupleStr; 
		     R -> R ++ ", " ++ TupleStr
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
    


%% @doc create build url fun
%% -spec create_build_url_fun(Rules::[rule()]) -> fun()
create_build_url_fun(DispRules) ->
    fun(Id, Args) ->
	    build_url(Id, Args, DispRules)
    end.




%% @doc build url
%% -spec create_build_url_fun(Id::atom(), Args::[{Key::atom(), Val::string()}], [rule()]) -> 
%%       			  {ok, Url::string()} | {error, term()}
build_url(Id, Args, DispRules) ->
    Checked = lists:all(
		fun({Key, Value}) when is_atom(Key), is_list(Value)->
			true;
		   (_)-> false
		end,
		Args),
    case Checked of
	true ->
	    case  get_path_loop(DispRules, Id) of
		undefined -> undefined;
		Path ->
		    url_append_items(Path, Args, [])
	    end;
	false ->
	    {error, "Args. Type not vaild"}
    end.
	

get_path_loop([], _Id)->
    undefined; 
get_path_loop([{Id, Path,_}|_], Id) ->
    Path;
get_path_loop([_|Tail], Id) ->
    get_path_loop(Tail, Id).


url_append_items([], _args, Acc) ->
    "/" ++ string:join(lists:reverse(Acc), "/");

url_append_items([E|T], Args, Acc) ->
    Val =
	case E of
	    E when is_atom(E) ->
		case proplists:get_value(E, Args) of
		    V when V /= undefined ->
			V
		end;
	    E when is_list(E)->
		E
	end,
    url_append_items(T, Args, [Val|Acc]).

    
	    
		  
