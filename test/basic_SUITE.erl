-module(basic_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([test/1]).

all() -> [test].

init_per_testcase(test, Config) ->
    application:start(inets),

    MininoPriv = code:priv_dir(minino),
    MininoBinFilename = filename:join([MininoPriv, "..", "bin", "minino"]),
    io:format("MininoBinFilename: ~p~n", [MininoBinFilename]),
    TestPriv = proplists:get_value(priv_dir, Config),
    CpCmd = lists:flatten(io_lib:format("cp ~s ~s", [MininoBinFilename, TestPriv])),
    os:cmd(CpCmd),
    AppCmd = lists:flatten(
	       io_lib:format("cd ~s; ./minino create-app id=kitty; ./rebar get-deps compile", [TestPriv])),
    os:cmd(AppCmd),

    %% add ebin kitty to path
    code:add_path(filename:join(TestPriv, "ebin")),

    %% set settings.cfg file
    Settings = filename:join([TestPriv, "priv", "settings.cfg"]),
    application:set_env(minino, settings_file, Settings),      
    
    %%start minino
    minino:start(),
    Config.

end_per_testcase(test, _Config) ->
    ok.


test(_Config) ->
    {ok, _} = httpc:request("http://127.0.0.1:8000"),
     %% io:format("response:  ~p~n", [R]),
    ok.



    
