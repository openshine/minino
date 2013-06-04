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

-module(minino_escript).

%%{Name::string(), Help()::string(), Available:: always | appcreated
-define(COMMANDS, 
	[
	 %%{Name::string(), Help()::string(), Available:: always | appcreated
	 {"create-app", "create a new app; create-app id=myapp", always},
	 {"runserver", "runserver [port]", appcreated},
	 {"debug", "debug [port]", appcreated},
	 {"compile", "compile", appcreated}

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
	false ->
	    usage(Commands)
    end.



is_command([],_) ->
    false;
is_command([Command|_], {AvailableCommnads, _}) ->
    is_command_loop(Command, AvailableCommnads).
	
is_command_loop(_Command, []) -> 
    false;
is_command_loop(Command, [{Command, _, _}|_]) -> 
    true;
is_command_loop(Command, [_Head|Commands]) -> 
    is_command_loop(Command, Commands). 

%% noapp | appcreated
get_status()->
    case filelib:wildcard(filename:join("src", "*.erl")) of
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
	    io:format("These commands will be available after app is created~n~n"),
	    print_commands(NoAvailableCommands)
    end.

print_commands(Commands) ->
    lists:foreach(
      fun({Name, Help, _A}) ->
	      io:format("~s\t\t\t~s~n", [Name, Help])
      end,
      Commands).


command(CommandArgs)->
    case CommandArgs of
	["create-app",[$i,$d,$=|AppName]] ->
	    create_app(AppName);
	["runserver" | PortList] ->
	    runserver(PortList, nodebug);
     	["debug" | PortList] ->
	    runserver(PortList, debug);
	["compile"|_Args] ->
	    compile_files(),
	    io:format("~n");
	_ -> notavailable
    end.


runserver(PortList, Mode) ->
    case PortList of
	[PStr] when is_list(PStr) ->	
	    {Port, _} = string:to_integer(PStr),
	    application:set_env(minino, mport, Port);
	_ -> ignore
    end,
    compile_files(),
    ok = minino:start(),
    case Mode of 
	nodebug -> timer:sleep(infinity);
	debug -> debug_mode()
    end.



create_app(AppName) ->
    io:format("~s app created.~n", [AppName]),

    %%create app
    AppFileName = filename:join(["src", AppName ++ ".erl"]),
    case filelib:is_regular(AppFileName) of
	true -> 
	    ignore;
	false ->
	    AppBin = get_bin("template.application.erl"),
	    AppCtx = dict:from_list([{application, AppName}]),
	    AppStr = render(binary_to_list(AppBin), AppCtx),
	    filelib:ensure_dir(AppFileName),
	    file:write_file(AppFileName, list_to_binary(AppStr))
    end,

    %%create app.src
    AppSrcFileName = filename:join(["src", AppName ++ ".app.src"]),
    case filelib:is_regular(AppSrcFileName) of
	true -> 
	    ignore;
	false ->
	    AppSrcBin = get_bin("template.application.app.src"),
	    AppSrcCtx = dict:from_list([{application, AppName}]),
	    AppSrcStr = render(binary_to_list(AppSrcBin), AppSrcCtx),
	    filelib:ensure_dir(AppSrcFileName),
	    file:write_file(AppSrcFileName, list_to_binary(AppSrcStr))
    end,

    %% create settings.cfg
    SettingsFileName = filename:join(["priv", "settings.cfg"]),
    case filelib:is_regular(SettingsFileName) of
	true -> 
	    ignore;
	false ->
	    SetBin = get_bin("template.settings.cfg"),
	    SetCtx = dict:from_list([{application, AppName},
				     {random_string, create_random_string()}]),
	    SetStr = render(binary_to_list(SetBin), SetCtx),
	    filelib:ensure_dir(SettingsFileName),
	    file:write_file(SettingsFileName, list_to_binary(SetStr))
    end,

    %% create html templates
    HomeTemplatePath = filename:join(["priv", "templates", "home.html"]),
    case filelib:is_regular(HomeTemplatePath) of
	true -> 
	    ignore;
	false ->
	    HomeBin = get_bin("template.home.html"),
	    filelib:ensure_dir(HomeTemplatePath),
	    file:write_file(HomeTemplatePath, HomeBin)
    end.

get_bin(FileName) ->
    {ok, PropList} = escript:extract("minino", []),
    Archive = proplists:get_value(archive, PropList),
    {ok, [{FileName,Bin}]} = 
	zip:foldl(
	  fun(FileInArchive, _GetInfo, GetBin, Acc) ->
		  case FileInArchive of
		      FileName -> [{FileInArchive, GetBin()}|Acc];
		      _ -> Acc
		  end		      
	  end,
	  [],
	  {"minino", Archive}),
    Bin.

render(Bin, Context) ->
    ReOpts = [global, {return, list}],
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Context).


create_random_string()->
       create_random_string(50, 1, []).

create_random_string(Length, Counter, Acc) when Length == Counter ->
    Acc;

create_random_string(Length, Counter, Acc)->
    FirstChar = 64,
    LastChar = 122,
    Char = random:uniform(LastChar - FirstChar + 1) + FirstChar - 1,
    create_random_string(Length, Counter + 1, [Char|Acc]).
    


compile_files()->
    Files = filelib:wildcard("src/*.erl"),
    ok = filelib:ensure_dir("ebin/dummy.file"),
    Opts = [verbose,
	    report_errors,
	    report_warnings,
	    {i, ["include"]},
	    {outdir, "ebin"}
	    
	   ],
    lists:foreach(
      fun(Path) ->
	      Result = compile:file(Path, Opts),
	      io:format("compile ~p -> ~p", [Path, Result]),
	      {ok, _Mod} = Result
      end,
      Files).

debug_mode() ->
    spawn(fun user_drv:start/0),
    erlang:register(minino_shell, self()),
    receive
	stop -> ignore
    end.
