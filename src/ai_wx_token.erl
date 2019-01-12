-module(ai_wx_token).
-include_lib("aihttp/include/ai_url.hrl").
-include("priv/ai_wx_internal.hrl").

-export([access_token/2,ticket_token/2]).

-spec access_token(AppID::binary(),Secret::binary()) -> list().
access_token(AppID,Secret)->
    R  = ai_url:parse(?ACCESS_TOKEN_PATH),
    R0 = R#ai_url{
        qs = [{<<"grant_type">>,<<"client_credential">>},{<<"appid">>,AppID},{<<"secret">>,Secret}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).
    
-spec ticket_token(AccessToken::binary(),Type::binary()) -> list().
ticket_token(AccessToken,Type)->
    R  = ai_url:parse(?TICKET_TOKEN_PATH),
    R0 = R#ai_url{
        qs = [{<<"type">>,Type},{<<"access_token">>,AccessToken}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).