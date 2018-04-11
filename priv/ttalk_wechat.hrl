-define(WECHAT_API_HOST,<<"https://api.weixin.qq.com">>).
-define(WECHAT_ACCESS_TOKEN_PATH,<<"/cgi-bin/token">>).
-define(WECHAT_TICKET_PATH,<<"/cgi-bin/ticket/getticket">>).

-define(WECHAT_TICKET_TYPE_JS,<<"jsapi">>).
-define(WECHAT_TICKET_TYPE_CARD,<<"wx_card">>).


-record(ttalk_wechat_token,{
	  app_id,
	  type,
	  token,
	  expired_at
	 }).
-record(ttalk_wechat_app,{
	  app_id,
	  secret
	 }).
