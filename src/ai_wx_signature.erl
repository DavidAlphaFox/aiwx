-module(ai_wx_signature).

-export([verify/3]).
-export([sign_jsapi/4]).

verify(Signature, Timestamp, Nonce) ->
    Token = ai_wx_conf:app_token(),
    Timestamp0 = ai_string:to_iolist(Timestamp),
    Nonce0 = ai_string:to_iolist(Nonce),
    TmpList = [Token, Timestamp0, Nonce0],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    Hash = ai_string:sha_string(TmpStr,lower),
    string:equal(Signature, Hash).

sign_jsapi(Ticket,Timestamp,Nonce,Url)->
    TmpList = [Ticket, Timestamp, Nonce,Url],
    TmpList2 = lists:sort(TmpList),
    TmpStr = string:join(TmpList2, ""),
    ai_string:sha_string(TmpStr,lower).
