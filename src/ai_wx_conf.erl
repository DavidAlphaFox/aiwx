-module(ai_wx_conf).

-export([start/1]).
-export([app_token/1,app_secret/1,app_id/1,app_key/1]).

-callback app_token(Context :: term()) -> string().
-callback app_id(Context :: term())-> string().
-callback app_secret(Context :: term())-> string().
-callback app_key(Context :: term())-> string().



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

app_token(Ctx)->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_token(Ctx).

app_id(Ctx)->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_id(Ctx).

app_secret(Ctx)->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_secret(Ctx).
app_key(Ctx)->
    Backend = ai_wx_conf_module:backend(),
    Backend:app_key(Ctx).
