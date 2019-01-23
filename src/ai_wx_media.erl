-module(ai_wx_media).
-include_lib("aihttp/include/ai_url.hrl").
-include("priv/ai_wx_internal.hrl").

-export([upload/3]).

%% 临时素材
upload(AccessToken,Type,File)->
    R  = ai_url:parse(?MEDIA_UPLOAD_PATH),
    Type0 = type(Type),
    R0 = R#ai_url{
        qs = [{<<"access_token">>,AccessToken},{<<"type">>,Type0}]
    },
    URL = ai_url:build(R0),
    ai_wx_http:post_multipart(?API_HOST,URL,File).

type(image)-> <<"image">>;
type(voice)-> <<"voice">>;
type(video)-> <<"video">>;
type(thumb)-> <<"thumb">>;
type(Type) -> Type.

