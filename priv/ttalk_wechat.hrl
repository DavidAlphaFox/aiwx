-define(API_HOST,<<"https://api.weixin.qq.com">>).
-define(ACCESS_TOKEN_PATH,<<"/cgi-bin/token">>).
-define(TICKET_TOKEN_PATH,<<"/cgi-bin/ticket/getticket">>).

-define(TOKEN_TYPE_ACCESS,<<"access_token">>).
-define(TOKEN_TYPE_JS,<<"jsapi">>).
-define(TOKEN_TYPE_CARD,<<"wx_card">>).


-record(wechat_token,{
	  token_key :: {binary(),binary()}, %% {app_id,type}
	  token :: {binary(),binary()},
	  expired_at :: integer()
	 }).

-record(wechat_app,{
	  app_id :: binary(),
	  secret :: binary()
	 }).
