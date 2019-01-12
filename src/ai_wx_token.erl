-module(ai_wx_token).
-include_lib("aihttp/include/ai_url.hrl").
-include("priv/ai_wx_internal.hrl").

-export([access_token_url/2,ticket_token_url/2]).

-spec access_token_url(AppID::binary(),Secret::binary()) -> list().
access_token_url(AppID,Secret)->
    R  = ai_url:parse(?API_HOST),
    R0 = R#ai_url{
        path = ?ACCESS_TOKEN_PATH,
        qs = [{<<"grant_type">>,<<"client_credential">>},{<<"appid">>,AppID},{<<"secret">>,Secret}]
    },
    ai_url:build(R0).
    
-spec ticket_token_url(AccessToken::binary(),Type::binary()) -> list().
ticket_token_url(AccessToken,Type)->
    R  = ai_url:parse(?API_HOST),
    R0 = R#ai_url{
        path = ?TICKET_TOKEN_PATH,
        qs = [{<<"type">>,Type},{<<"access_token">>,AccessToken}]
    },
    ai_url:build(R0).