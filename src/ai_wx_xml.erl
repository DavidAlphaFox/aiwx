-module(ai_wx_xml).
-include_lib("xmerl/include/xmerl.hrl").

-export([text/4]).

-define(TEXT_ELEM,#xmlElement{name = 'MsgType',content =
    [#xmlText{type = cdata,value = <<"text">>}]}).

text(MsgID,FromUser,ToUser,Content)->
    FromEl = #xmlElement{name = 'FromUserName',
        content =[#xmlText{type = cdata,value = ai_string:to_string(FromUser)}]},
    ToEl = #xmlElement{name = 'ToUserName',
        content =[#xmlText{type = cdata,value = ai_string:to_string(ToUser)}]},
    {M,S,_} = os:timestamp(),
    Stamp = M * 1000000 + S,
    CreateTimeEl = #xmlElement{name = 'CreateTime',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(Stamp)}]},
    MsgIDEl = #xmlElement{name = 'MsgId',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(MsgID)}]},
    ContentEl = #xmlElement{name = 'Content',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(Content)}]},
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = [FromEl,ToEl,CreateTimeEl,?TEXT_ELEM,ContentEl,MsgIDEl]},
        xmerl_xml)).   
    