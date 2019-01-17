-record(ai_wx_button,{
    sub_button = undefined,
    type = undefined, %% 菜单的响应动作类型，view表示网页类型，click表示点击类型，miniprogram表示小程序类型
    name = undefined, %% 菜单标题，不超过16个字节，子菜单不超过60个字节
    key = undefined, %%click等点击类型必须	菜单KEY值，用于消息接口推送，不超过128字节
    url = undefined, 
    %%view、miniprogram类型必须	
    %%网页 链接，用户点击菜单可打开链接，不超过1024字节。 
    %%type为miniprogram时，不支持小程序的老版本客户端将打开本url。
    media_id = undefined,
    %% media_id类型和view_limited类型必须	调用新增永久素材接口返回的合法media_id
    appid = undefined,
    %% miniprogram类型必须	小程序的appid（仅认证公众号可配置）
    pagepath = undefined
    %% miniprogram类型必须
}).
