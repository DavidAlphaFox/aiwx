# 微信公众平台SDK

## 说明
由于[微信公众平台(wechat)](https://mp.weixin.qq.com)并未提供官方的Erlang版本的SDK，因此才编写的这个Erlang版本的SDK，用来完成微信公众平台所有的操作

## 功能
    
[x]URL接入  
[x]AccessToken获取  
[x]消息加密     
[x]接收普通消息  
[x]接收事件消息

## 如何使用

### 依赖

因使用xemrl，要求最低使用Erlang OTP 20来运行该项目  
因使用erlang.mk进行的构建管理，因此需要 GNU Make 4 及以上来进行构建

依赖的类库

* cowlib
* gun
* ranch
* cowboy
* poolboy
* jsx
* ailib
* aihttp

### 使用

#### 配置微信相关信息

在项目启动后需要使用ai_wx_conf来进行微信配置

    ai_wx_conf:start(Module)

其中的`Module`为包含微信配置信息的模块，该模块需要支持以下行为

    -callback app_token(Context) -> string().
    -callback app_id(Context)-> string().
    -callback app_secret(Context)-> string().
    -callback app_key(Context)-> string().

`Context` 是一个获取各配置的上下文，需要使用者去指定

* `app_id`为`开发/基本配置/公众号开发信息`中的`开发者ID(AppID)`
* `app_secret`为`开发/基本配置/公众号开发信息`中的`开发者密(AppSecret)`
* `app_token`为`开发/基本配置/服务器配置`中的`令牌(Token)`
* `app_key`为`开发/基本配置`中的`消息加解密密钥(EncodingAESKey)`


#### 消息处理handler

ai_wx_message_handler可以用来处理`接收普通消息`和`接收事件消息`

在cowboy的路径中进行如下配置

    {"/wx/message",ai_wx_message_handler,#{handler => Module,encrypt => true}}

`encrypt`用来指明是`ai_wx_message_handler`收到的消息是否加密，该值默认为true，代表加密模式。该模式可以从`开发/基本配置/消息加解密方式`中进行设置，默认设置为`明文模式`。
加密模式下可以将`开发/基本配置/消息加解密方式`设置为`安全模式`或`混合模式`，非加密模式可以将`开发/基本配置/消息加解密方式`设置为`明文模式`。

`Module`为项目中真正处理用户消息的模块，该模块需要支持以下行为

    -callback init(Req,State) -> {ok,Req,Context}.
    -callback handle_message(string(),maps:maps(),Context) -> binary().
    -callback handle_event(string(),maps:maps(),Context)-> binary().

* `init`是请求初始化阶段调用的，该阶段消息并未验证有效性，需要从Req中获得配置及后面处理事件的赏析文。
* `handle_message`用来处理用户消息，第一个参数为MsgType
* `handle_event`用来处理关注，扫码，菜单等事件，第一个参数为Event

它们都会接受一个map作为参数，该map为微信公众平台[消息管理](https://mp.weixin.qq.com/wiki?t=resource/res_main&id=mp1421140453)中描述的消息的键值对，如下面例子

    原始XML
    <xml>  
        <ToUserName>< ![CDATA[toUser] ]></ToUserName> 
        <FromUserName>< ![CDATA[fromUser] ]></FromUserName> 
        <CreateTime>1348831860</CreateTime> 
        <MsgType>< ![CDATA[text] ]></MsgType> 
        <Content>< ![CDATA[this is a test] ]></Content>        
        <MsgId>1234567890123456</MsgId> 
    </xml>
    转化后的Map
    #{
        'ToUserName' => "toUser",
        'FromUserName' => "fromUser",
        'CreateTime' => 1348831860,
        'MsgType' => "text",
        'Content' => "this is a test",
        'MsgId' => 1234567890123456
    }

它们的返回值，将作为回复给用户的消息，这个消息可以使用`ai_wx_xml`来进行构建。
