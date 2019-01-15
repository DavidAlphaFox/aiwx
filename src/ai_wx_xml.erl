-module(ai_wx_xml).
-include_lib("xmerl/include/xmerl.hrl").

-export([message/5,encrypted_message/4]).

-define(TEXT_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"text">>}]}).
-define(IMAGE_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"image">>}]}).
-define(VOICE_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"voice">>}]}).
-define(VIDEO_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"video">>}]}).
-define(MUSIC_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"music">>}]}).
-define(NEWS_ELEM,#xmlElement{name = 'MsgType',content =
                                  [#xmlText{type = cdata,value = <<"news">>}]}).

message(text,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    ContentEl = #xmlElement{name = 'Content',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(Content)}]},
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = [?TEXT_ELEM,ContentEl|BaseElts]},
        xmerl_xml));   
message(image,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    ContentEl = #xmlElement{name = 'MediaId',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(Content)}]},
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = [?IMAGE_ELEM,ContentEl|BaseElts]},
        xmerl_xml));
message(voice,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    ContentEl = #xmlElement{name = 'MediaId',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(Content)}]},
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = [?VOICE_ELEM,ContentEl|BaseElts]},
        xmerl_xml)); 
message(video,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    MediaID = maps:get('MediaId',Content),
    MediaIDEl = #xmlElement{name = 'MediaId',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(MediaID)}]},
    Contents = [?VIDEO_ELEM,MediaIDEl|BaseElts],
    Contents0 = 
        case maps:get('Title',Content,undefined) of 
            undefined -> Contents;
            Title ->
                TitleEl = #xmlElement{name = 'Title',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(Title)}]},
                [TitleEl|Contents]
        end,
    Contents1 = 
        case maps:get('Description', Content,undefined) of 
            undefined -> Contents0;
            Description ->
                DescEl = #xmlElement{name = 'Description',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(Description)}]},
                    [DescEl|Contents0]
        end,
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = Contents1 },
        xmerl_xml));               
message(music,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    ThumbMediaID = maps:get('ThumbMediaId',Content),	
    ThumbMediaIDEl = #xmlElement{name = 'ThumbMediaId',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(ThumbMediaID)}]},
    Contents = [?MUSIC_ELEM,ThumbMediaIDEl|BaseElts],
    Contents0 = 
        case maps:get('Title',Content,undefined) of 
            undefined -> Contents;
            Title ->
                TitleEl = #xmlElement{name = 'Title',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(Title)}]},
                [TitleEl|Contents]
        end,
    Contents1 = 
        case maps:get('Description', Content,undefined) of 
            undefined -> Contents0;
            Description ->
                DescEl = #xmlElement{name = 'Description',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(Description)}]},
                [DescEl|Contents0]
        end,
    Contents2 = 
        case maps:get('MusicURL',Content,undefined) of 
            undefined -> Contents1;
            MusicURL->
                MusicURLEl = #xmlElement{name = 'MusicURL',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(MusicURL)}]},
                [MusicURLEl|Contents1]
        end,
    Contents3 = 
        case maps:get('HQMusicUrl',Content,undefined) of 
            undefined -> Contents2;
            HQMusicURL->
                HQMusicURLEl = #xmlElement{name = 'HQMusicUrl',      
                    content =[#xmlText{type = cdata,value = ai_string:to_string(HQMusicURL)}]},
                [HQMusicURLEl|Contents2]
        end,
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = Contents3 },
        xmerl_xml)); 
message(news,MsgID,FromUser,ToUser,Content)->
    BaseElts = base_elements(MsgID,FromUser,ToUser),
    ArticleCount = maps:get('ArticleCount',Content),
    ArticleCountEl = #xmlElement{name = 'ArticleCount',      
        content =[#xmlText{type = cdata,value = ai_string:to_string(ArticleCount)}]},
    Articles = maps:get('Articles',Content),
    ArticlesEl = articles(Articles),
    ai_string:to_string(xmerl:export_element(
        #xmlElement{name = xml,content = [?NEWS_ELEM,ArticleCountEl,ArticlesEl|BaseElts] },
        xmerl_xml)).
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
    
base_elements(MsgID,FromUser,ToUser)->
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
    [FromEl,ToEl,CreateTimeEl,MsgIDEl].
articles(Articles)->
    ArticleElts = 
        ai_lists:foldr(fun(I,Acc)->
            Title = maps:get('Title',I),
            Description = maps:get('Description', I), 
            URL = maps:get('Url',I),
            PicURL = maps:get('PicUrl',I),

            TitleEl = #xmlElement{name = 'Title',      
                content =[#xmlText{type = cdata,value = ai_string:to_string(Title)}]},
            DescEl = #xmlElement{name = 'Description',      
                content =[#xmlText{type = cdata,value = ai_string:to_string(Description)}]},
            PicURLEl = #xmlElement{name = 'PicUrl',      
                content =[#xmlText{type = cdata,value = ai_string:to_string(PicURL)}]},
            URLEl = #xmlElement{name = 'Url',      
                content =[#xmlText{type = cdata,value = ai_string:to_string(URL)}]},
            [#xmlElement{name = item, content =[TitleEl,DescEl,PicURLEl,URLEl]}|Acc]
        end,[],Articles),
    #xmlElement{name = 'Articles',content =ArticleElts}.