-module(ai_wx_message_handler).

-export([init/2]).

init(#{method := <<"GET">>} = Req, State) ->
    handle(get,Req,State).


handle(get,Req, State) ->
  QS = cowboy_req:parse_qs(Req),
  Signature = proplists:get_value(<<"signature">>,QS),
  Timestamp = proplists:get_value(<<"timestamp">>,QS),
  Nonce = proplists:get_value(<<"nonce">>,QS),
  EcohStr = proplists:get_value(<<"echostr">>,QS),
  Verify = ai_wx_signature:verify(Signature, Timestamp, Nonce),
  Req2 = 
    if
    Verify == false ->
        cowboy_req:reply(200, [
          {<<"content-type">>, <<"text/plain">>}
        ], <<"">>, Req);
    true ->
      cowboy_req:reply(200, [
        {<<"content-type">>, <<"text/plain">>}
      ], EcohStr, Req)
  end,
  {ok, Req2, State}.
