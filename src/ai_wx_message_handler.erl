-module(ai_wx_message_handler).
-include_lib("xmerl/include/xmerl.hrl").

-export([init/2]).

init(#{method := <<"GET">>} = Req, State) ->
    handle(get,Req,State);
init(#{method := <<"POST">>} = Req,State)->
	handle(post,Req,State).

handle(post,Req,State)->
	{ok,Body,Req0} = read_body(Req),
	XmlBody = ai_string:to_iolist(Body),
	{XmlElt, _} = xmerl_scan:string(XmlBody),
	Content = XmlElt#xmlElement.content,
	Map = 
		lists:foldl(fun(El, Acc) ->
			Key = El#xmlElement.name,
			[ElContent] = El#xmlElement.content,
			Value = ElContent#xmlText.value,
			Acc#{Key => Value}
		end,#{}, Content),
	case maps:get(handler,State,undefined) of 
		undefined-> 
			Req1 = cowboy_req:reply(200, 
					#{<<"content-type">> => <<"text/plain">>}, <<"">>, Req0),
			{ok,Req1,State};
		Handler -> 
			Reply = Handler:handle(Map),
			Req1 = cowboy_req:reply(200,
				#{<<"content-type">> => <<"application/xml">>},Reply,Req0),
			{ok,Req1,State}
	end;
	
handle(get,Req, State) ->
  QS = cowboy_req:parse_qs(Req),
  Signature = proplists:get_value(<<"signature">>,QS),
  Timestamp = ai_string:to_iolist(proplists:get_value(<<"timestamp">>,QS)),
  Nonce = ai_string:to_iolist(proplists:get_value(<<"nonce">>,QS)),
  EcohStr = proplists:get_value(<<"echostr">>,QS),
  Verify = ai_wx_signature:verify(Signature, Timestamp, Nonce),
  Req0 = 
    if
    Verify == false ->
        cowboy_req:reply(200, 
          #{<<"content-type">> => <<"text/plain">>}
        , <<"">>, Req);
    true ->
      cowboy_req:reply(200, [
        #{<<"content-type">> => <<"text/plain">>}
      ], EcohStr, Req)
  end,
  {ok, Req0, State}.

read_body(Req)->
    read_body(Req,<<>>).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> {ok, << Acc/binary, Data/binary >>, Req0};
        {more, Data, Req0} -> read_body(Req0, << Acc/binary, Data/binary >>)
	end.
	

