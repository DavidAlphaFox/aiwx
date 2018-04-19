-module(ttalk_wechat_token).
-include("priv/ttalk_wechat.hrl").
-export([access_token/2,ticket_token/2]).
-export([jsapi_ticket_token/1,card_ticket_token/1]).


-spec access_token(AppID::binary(),Secret::binary()) -> list().
access_token(AppID,Secret)->
    URL = hackney_url:make_url(?API_HOST,?ACCESS_TOKEN_PATH,
			       [{<<"grant_type">>,<<"client_credential">>},{<<"appid">>,AppID},{<<"secret">>,Secret}]),
    Body = ttalk_wechat_http:api_get(URL),
    jsx:decode(Body).
    
-spec ticket_token(AccessToken::binary(),Type::binary()) -> list().
ticket_token(AccessToken,Type)->
    URL = hackney_url:make_url(?API_HOST,?TICKET_TOKEN_PATH,
			       [{<<"type">>,Type},{<<"access_token">>,AccessToken}]),
    Body = ttalk_wechat_http:api_get(URL),
    jsx:decode(Body).
    
		
-spec jsapi_ticket_token(AccessToken::binary()) -> list().
jsapi_ticket_token(AccessToken)->
    ticket_token(AccessToken,?TOKEN_TYPE_JS).

-spec card_ticket_token(AccessToken::binary()) -> list().
card_ticket_token(AccessToken)->
    ticket_token(AccessToken,?TOKEN_TYPE_CARD).

