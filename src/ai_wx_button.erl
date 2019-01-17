-module(ai_wx_button).
-include("ai_wx.hrl").
-export([new/0,to_json/1]).

new() -> #ai_wx_button{}.

to_json(Buttons)->
    M = to_map(Buttons),
    Size = erlang:length(M),
    if 
        Size > 3 -> throw({error,{too_many,button}});
        true -> jsx:encode(#{<<"button">> => M})
    end.

to_map(Button) when erlang:is_record(Button,ai_wx_button)->
    M = to_map(Button,#{}),
    [M];
to_map(Buttons) when erlang:is_list(Buttons)->
    lists:map(fun(Button) ->
            to_map(Button,#{})
        end,Buttons).



to_map(Button,M)->
    Fields = record_info(fields,ai_wx_button),
    IndexedFields = lists:zip(Fields,lists:seq(2,erlang:length(Fields) + 1)),
    Type = Button#ai_wx_button.type,
    lists:foldl(fun({Field,Index},Acc)->
            Value = erlang:element(Index,Button),
            to_map(Type,Field,Value,Acc)
        end,M,IndexedFields).
to_map(_Type,name,undefined,_Acc)-> throw({error,{absent,name}});
to_map(_Type,sub_button,undefined,Acc)->Acc;
to_map(_Type,sub_button,Value,Acc)->
    SubButton = to_map(Value),
    Size = erlang:length(SubButton),
    if 
        Size > 5 -> throw({error,{too_many,sub_button}});
        true -> ok 
    end,
    Acc#{ai_string:to_string(sub_button) => SubButton};

to_map(click,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(location_select,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(pic_weixin,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(pic_photo_or_album,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(pic_sysphoto,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(scancode_push,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(scancode_waitmsg,key,undefined,_Acc)-> throw({error,{absent,key}});
to_map(view,url,undefined,_Acc)-> throw({error,{absent,url}});
to_map(miniprogram,url,undefined,_Acc)-> throw({error,{absent,url}});
to_map(miniprogram,appid,undefined,_Acc)-> throw({error,{absent,appid}});
to_map(miniprogram,pagepath,undefined,_Acc)-> throw({error,{absent,pagepath}});
to_map(media_id,media_id,undefined,_Acc)-> throw({error,{absent,media_id}});
to_map(view_limited,media_id,undefined,_Acc)-> throw({error,{absent,media_id}});
to_map(_Type,_Key,undefined,Acc) -> Acc;
to_map(_Type,Key,Value,Acc)->
    Acc#{ai_string:to_string(Key) => ai_string:to_string(Value)}.