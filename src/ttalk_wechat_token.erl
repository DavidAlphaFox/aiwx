-module(ttalk_wechat_token).
-include("priv/ttalk_wechat.hrl").
-export([request_access_token/2,request_ticket/2]).
-export([request_jsapi_ticket/1,request_card_ticket/1]).
-spec request_access_token(AppID::binary(),Secret::binary()) -> {ok,binary()}.
request_access_token(AppID,Secret)->
		URL = hackney_url:make_url(?WECHAT_API_HOST,?WECHAT_ACCESS_TOKEN_PATH,
															 [{<<"grant_type">>,<<"client_credential">>},
																{<<"appid">>,AppID},
																{<<"secret">>,Secret}]),
		{ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get,URL),
		hackney:body(ClientRef).

-spec request_ticket(AccessToken::binary(),Type::binary()) -> {ok,binary()}.
request_ticket(AccessToken,Type)->
		URL = hackney_url:make_url(?WECHAT_API_HOST,?WECHAT_TICKET_PATH,
															 [{<<"type">>,Type},
																{<<"access_token">>,AccessToken}]),
		{ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get,URL),
		hackney:body(ClientRef).
		
-spec request_jsapi_ticket(AccessToken::binary()) -> {ok,binary()}.
request_jsapi_ticket(AccessToken)->
		request_ticket(AccessToken,<<"jsapi">>).

-spec request_card_ticket(AccessToken::binary()) -> {ok,binary()}.
request_card_ticket(AccessToken)->
		request_ticket(AccessToken,<<"wx_card">>).
