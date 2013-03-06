%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  6 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_escript).

%%{Name::string(), Help()::string(), Available:: always | appcreated
-define(COMMANDS, 
	[
	 %%{Name::string(), Help()::string(), Available:: always | appcreated
	 {"create-app", "create a new app; create-app id=myapp", always},
	 {"build", "build minino", appcreated},
	 {"clean", "clean minino", appcreated},
	 {"start", "start minino", appcreated},
	 {"stop", "stop minino", appcreated}
	]).

%% API
-export([main/1]).


main(Args) ->
    Status = get_status(),
    Commands = get_commands(Status),
    {ok, {[], CommandArgs}} = getopt:parse([], Args),
    case is_command(CommandArgs, Commands) of
	true -> 
	    case command(CommandArgs) of
		ok -> ok;
		notavailable -> usage(Commands)
	    end;
	false -> usage(Commands)
    end.



is_command([],_) ->
    false;
is_command([Command|_], {AvailableCommnads, _}) ->
    is_command_loop(Command, AvailableCommnads).
	
is_command_loop(_Command, []) -> 
    false;
is_command_loop(Command, [{Command, _}|_]) -> 
    true;
is_command_loop(Command, [_Head|Commands]) -> 
    is_command_loop(Command, Commands). 

%% noapp | appcreated
get_status()->
    case filelib:wildcard("*.erl") of
	[] -> noapp;
	_ -> appcreated
    end.
 
get_commands(Status)->
    Acc0 = {[],[]},
    lists:foldr(
      fun({_Command, _Help, always}=C, {CAvai, CNoAvai})->
      	      {[C|CAvai], CNoAvai};
      	 (C, {CAvai, CNoAvai})->	      
      	      case Status of
      	      	  appcreated -> 
      	      	      {[C|CAvai], CNoAvai};
      	      	  _Esle ->
      		      {CAvai, [C|CNoAvai]}
      	      end
      end,
      Acc0, 
      ?COMMANDS).


usage({AvailableCommands, NoAvailableCommands}) ->  
    io:format("~n"),
    io:format("minino commands: ~n~n"),
    print_commands(AvailableCommands),
    case NoAvailableCommands of
	[] -> ok;
	NoAvailableCommands ->
	    io:format("~n"),
	    io:format("no app was created yet. Please create one~n"),
	    io:format("No available commands. They will be available after app is created~n~n"),
	    print_commands(NoAvailableCommands)
    end.

print_commands(Commands) ->
    lists:foreach(
      fun({Name, Help, _A}) ->
	      io:format("~s\t\t\t~s~n", [Name, Help])
      end,
      Commands).

command(_CommandsArgs)->
    notavailable.
