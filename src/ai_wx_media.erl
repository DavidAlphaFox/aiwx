-module(ai_wx_media).
-include_lib("aihttp/include/ai_url.hrl").
-include("priv/ai_wx_internal.hrl").

-export([upload_media/3,get_media/2,get_jssdk_media/2]).

%% 临时素材
upload_media(AccessToken,Type,File)->
    R  = ai_url:parse(?MEDIA_UPLOAD_PATH),
    Type0 = type(Type),
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken},{<<"type">>,Type0}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:post_multipart(?API_HOST,URL,File).

%% 获取临时素材
get_media(AccessToken,MediaID)->
    R  = ai_url:parse(?MEDIA_GET_PATH), 
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken},{<<"media_id">>,MediaID}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).
%% 获取JSSDK上传的内容
get_jssdk_media(AccessToken,MediaID)->
    R  = ai_url:parse(?MEDIA_GET_JSSDK_PATH), 
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken},{<<"media_id">>,MediaID}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:get(?API_HOST,URL).
        

type(image)-> <<"image">>;
type(voice)-> <<"voice">>;
type(video)-> <<"video">>;
type(thumb)-> <<"thumb">>;
type(Type) -> Type.

