-module(ai_wx_menu).

-include_lib("aihttp/include/ai_url.hrl").
-include("priv/ai_wx_internal.hrl").

-export([create/2, retrieve/1, delete/1]).


create(AccessToken, Menu) ->
    R  = ai_url:parse(?MENU_CREATE_PATH),
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken}]
    },
    URL = ai_url:build(R0),
    MenuBinary = ai_wx_button:to_json(Menu),
    ai_wx_http:post(?API_HOST,URL,MenuBinary).

retrieve(AccessToken) ->
    R  = ai_url:parse(?MENU_RETRIEVE_PATH),
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).

delete(AccessToken) ->
    R  = ai_url:parse(?MENU_DELETE_PATH),
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).