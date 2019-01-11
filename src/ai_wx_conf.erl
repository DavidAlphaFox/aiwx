-module(ai_wx_conf).
-export([start/1]).
-export([app_token/0,app_secret/0,app_id/0]).
-spec start(atom())-> ok.
start(M)->
    Module = erlang:atom_to_list(M),
    {module,ai_wx_conf_module} =
        ai_string:dynamic_module("ai_wx_conf_module.erl",backend_module(Module)),
    ok.
backend_module(M) ->
    lists:flatten(
      ["-module(ai_wx_conf_module).
        -export([backend/0]).
        -spec backend() -> atom().
        backend() ->",M,".\n"]).

app_token()->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_token().

app_id()->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_id().
app_secret()->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_secret().