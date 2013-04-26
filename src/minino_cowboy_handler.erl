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


init(_Transport, CReq, []) ->
    {ok, CReq, undefined}.


handle(CReq, State) ->
    {ok, MReq} = minino_sessions:get_or_create(#mreq{creq=CReq}),
    %% MReq1 = write_multipart_to_file(MReq),
    MReq2 = minino_dispatcher:dispatch(MReq),
    CReqRespose = MReq2#mreq.creq,
    {ok, CReqRespose, State}.

terminate(_Reason, _Req, _State) ->
    ok.

write_multipart_to_file(MReq)->
    write_multipart_to_file_loop(MReq, [], []).

%% write_file
write_multipart_to_file_loop(MReq, Fd, Path) ->
    R = try 
	    cowboy_req:multipart_data(MReq#mreq.creq)
	catch _:_ -> no_multipart
	end,
    case R of
	{headers, _Headers, NewCReq} ->
	    TmpFilePath = get_tmp_path(),
	    filelib:ensure_dir(TmpFilePath),
	    {ok, Fd1}  = file:open(TmpFilePath ,[write, raw]),
	    write_multipart_to_file_loop(MReq#mreq{creq=NewCReq}, 
					 Fd1, 
					 TmpFilePath);
	{body, Data, NewCReq} ->
	    ok = file:write(Fd, Data),
	    write_multipart_to_file_loop(MReq#mreq{creq=NewCReq}, 
					 Fd, 
					 Path);
	{end_of_part, NewCReq}->
	    ok = file:close(Fd),
	    #mreq{creq=NewCReq,file=Path};
	no_multipart ->
	    MReq
    end.


get_tmp_path() ->
    Filename = random_md5(),
    {ok, MConf} = minino_config:get(),
    Dir = proplists:get_value(uploadfiles, MConf, "priv/tmp"),
    filename:join([Dir, Filename]).


random_md5() ->
    Str = lists:flatten(io_lib:format("~p", [now()])),
    Md5 = erlang:md5(Str),
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Md5]).
