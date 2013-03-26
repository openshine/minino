%%%-------------------------------------------------------------------
%%% @author Pablo Vieytes <pvieytes@openshine.com>
%%% @copyright (C) 2013, Openshine s.l.
%%% @doc
%%%
%%% @end
%%% Created :  15 Mar 2013 by Pablo Vieytes <pvieytes@openshine.com>
%%%-------------------------------------------------------------------
-module(minino_api).


-export([response/2,
	 path/1,
	 render_template/2]).

-type template_path() :: string().
-type template_args() :: [{atom(), string()}].




response({error, 404}, Req) ->
    CReq = proplists:get_value(cowreq, Req),
    {ok, Req2} = cowboy_req:reply(404, [], <<"Error 404: Not found">>, CReq),
    Req2;

response(Msg, Req) when is_list(Msg) ->
    CReq = proplists:get_value(cowreq, Req),
    {ok, Req2} = cowboy_req:reply(200, 
				  [], 
				  list_to_binary(Msg), 
				  CReq),
    Req2.


path(Req) ->
    CReq = proplists:get_value(cowreq, Req),
    {BinPath, _} = cowboy_req:path(CReq),
    string:tokens(binary_to_list(BinPath), "/").
	

%% @doc render a template.
-spec render_template(template_path(), template_args()) -> {ok, string()} | {error, term()}.
render_template(Template, Args) ->
    minino_templates:render(Template, Args).
