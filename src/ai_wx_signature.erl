-module(ai_wx_signature).

-export([verify/3,verify/4]).
-export([sign_message/3,sign_jsapi/4]).

verify(Signature, Timestamp, Nonce) ->
    Token = ai_wx_conf:app_token(),
    Timestamp0 = ai_string:to_iolist(Timestamp),
    Nonce0 = ai_string:to_iolist(Nonce),
    TmpList = [Token, Timestamp0, Nonce0],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    Hash = ai_string:sha_string(TmpStr,lower),
    string:equal(Signature, Hash).

verify(Signature,Timestamp,Nonce,Msg)->
    Token = ai_wx_conf:app_token(),
    Timestamp0 = ai_string:to_iolist(Timestamp),
    Nonce0 = ai_string:to_iolist(Nonce),
    Msg0 = ai_string:to_iolist(Msg),
    TmpList = [Token, Timestamp0, Nonce0,Msg0],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    Hash = ai_string:sha_string(TmpStr,lower),
    string:equal(Signature, Hash).

sign_message(Timestamp,Nonce,Msg)->
    Token = ai_wx_conf:app_token(),
    Timestamp0 = ai_string:to_iolist(Timestamp),
    Nonce0 = ai_string:to_iolist(Nonce),
    Msg0 = ai_string:to_iolist(Msg),
    TmpList = [Token, Timestamp0, Nonce0,Msg0],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    ai_string:sha_string(TmpStr,lower).

sign_jsapi(Ticket,Timestamp,Nonce,Url)->
    TmpList = [Ticket, Timestamp, Nonce,Url],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    ai_string:sha_string(TmpStr,lower).
