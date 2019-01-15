-module(ai_wx_xml).
-include_lib("xmerl/include/xmerl.hrl").

-export([message/5,encrypted_message/4]).

-define(TEXT_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"text">>}]}).
-define(LOC_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"location">>}]}).

message(text,MsgID,FromUser,ToUser,Content)->
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
        xmerl_xml));   
message(location,MsgID,FromUser,ToUser,Content)->
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
    LocX = maps:get('Location_X',Content),
    LocY = maps:get('Location_Y',Content),
    Label = maps:get('Label',Content),
    Scale = maps:get('Scale',Content),
    LocXEl = #xmlElement{name = 'Location_X',      
                         content =[#xmlText{type = cdata,value = ai_string:to_string(LocX)}]},
    LocYEl = #xmlElement{name = 'Location_Y',      
                         content =[#xmlText{type = cdata,value = ai_string:to_string(LocY)}]},
    LabelEl = #xmlElement{name = 'Label',      
                         content =[#xmlText{type = cdata,value = ai_string:to_string(Label)}]},
    ScaleEl = #xmlElement{name = 'Scale',      
                         content =[#xmlText{type = cdata,value = ai_string:to_string(Scale)}]},
    ai_string:to_string(xmerl:export_element(
                          #xmlElement{name = xml,
                                      content = [FromEl,ToEl,CreateTimeEl,?LOC_ELEM,LocXEl,LocYEl,LabelEl,ScaleEl,MsgIDEl]
                                     },xmerl_xml)).    
encrypted_message(Signature,Timestamp,Nonce,Msg)->
    TimeEl = #xmlElement{name = 'TimeStamp',      
                         content =[#xmlText{type = cdata,value = ai_string:to_string(Timestamp)}]},
    NonceEl = #xmlElement{name = 'Nonce',      
                          content =[#xmlText{type = cdata,value = ai_string:to_string(Nonce)}]},
    MsgEl = #xmlElement{name = 'Encrypt',      
                        content =[#xmlText{type = cdata,value = ai_string:to_string(Msg)}]},
    SignEl = #xmlElement{name = 'MsgSignature',      
                        content =[#xmlText{type = cdata,value = ai_string:to_string(Signature)}]},
    ai_string:to_string(xmerl:export_element(
                          #xmlElement{name = xml,
                                      content = [TimeEl,NonceEl,MsgEl,SignEl]
                                     },xmerl_xml)).   
    
