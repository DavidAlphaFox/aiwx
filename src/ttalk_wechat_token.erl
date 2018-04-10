-module(ttalk_wechat_token).
-include("priv/ttalk_wechat.hrl").
-export([request_access_token/2]).

-spec request_access_token(AppID::binary(),Secret::binary()) -> {ok,binary()}.
request_access_token(AppID,Secret)->
		URL = hackney_url:make_url(?API_HOST,?ACCESS_TOKEN_PATH,
															 [{<<"grant_type">>,<<"client_credential">>},
																{<<"appid">>,AppID},
																{<<"secret">>,Secret}]),
		{ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get,URL),
		hackney:body(ClientRef).

