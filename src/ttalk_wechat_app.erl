%%%-------------------------------------------------------------------
%% @doc ttalk_wechat public API
%% @end
%%%-------------------------------------------------------------------

-module(ttalk_wechat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
		application:start(unicode_util_compat),
		application:start(idna),
		application:ensure_all_started(hackney),
    ttalk_wechat_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
