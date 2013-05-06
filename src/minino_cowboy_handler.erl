%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  12 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------

-module(minino_cowboy_handler).
-include("include/minino.hrl").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-export([get_file/2]).

init(_Transport, CReq, []) ->
    {ok, CReq, undefined}.


handle(CReq, State) ->
    {ok, MReq} = minino_sessions:get_or_create(#mreq{creq=CReq}),
    Ref = minino_dispatcher:dispatch(MReq),
    {Msg, Code, MReq2} = receive_loop(Ref),
    {ok, NewCReq} = cowboy_req:reply(Code, 
				     [{<<"connection">>, <<"close">>}], 
				     Msg, 
				     MReq2#mreq.creq),
    {ok, NewCReq, State}.


terminate(_Reason, _Req, _State) ->
    ok.


receive_loop(Ref) ->
    receive
	{response, Ref, Response} ->
	    Response;
	{get_file, MReq, Path, GetFileRef, From} ->
	    Reply = save_file(MReq, Path),
	    From ! {get_file, GetFileRef, Reply},
	    receive_loop(Ref)
    end.



get_file(MReq, Path) ->
    Pid = MReq#mreq.from,
    Ref =   make_ref(),
    Pid ! {get_file, MReq, Path, Ref, self()},
    receive
	{get_file, Ref, Reply}	 ->
	    Reply
    after 100000 ->
	    {error, timeout}
    end.



save_file(MReq, Path) ->
    case is_secure_path(Path) of
	true ->
	    Multipart = try 
			    cowboy_req:multipart_data(MReq#mreq.creq)
			catch _:_ ->  no_multipart
			end,
	    case Multipart  of
		no_multipart -> {error, no_multipart};
		{headers, _Headers, NewCReq} ->
		    {ok, Fd}  = file:open(Path, [write, raw]),	
		    save_file_loop(NewCReq, Fd, Path);
		Else -> {error, Else}
	    end;
	false -> {error, "path not valid"}
    end.


save_file_loop(CReq, Fd, Path) ->
    case cowboy_req:multipart_data(CReq) of
	{body, Data, NewCReq} ->
	    ok = file:write(Fd, Data),
	    save_file_loop(NewCReq, Fd,  Path);
	{end_of_part, _NewCReq} ->
	    ok = file:close(Fd),
	    ok;
	Else -> {error, Else}
    end.


is_secure_path(Path) ->
    NoSecure = 
	lists:any(fun("..")-> true;
		     (_E)->false
		  end,
		  filename:split(Path)),
    not NoSecure.
