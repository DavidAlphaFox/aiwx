-module(ai_wx_message_handler).
-include_lib("xmerl/include/xmerl.hrl").

-export([init/2]).

init(Req,State)->
    QS = cowboy_req:parse_qs(Req),
    Verify = verify(QS),
    init(Verify,Req,State).
init(false,Req,State)->
    Req0 = cowboy_req:reply(200, 
                            #{<<"content-type">> => <<"text/plain">>},
                            <<"">>, Req),
    {ok,Req0,State};
init(true,#{method := <<"GET">>} = Req, State) ->
    handle(get,Req,State);
init(true,#{method := <<"POST">>} = Req,State)->
    handle(post,Req,State).

handle(post,Req,State)->
	{XmlElt,Req0} = decode(Req,State),
		io:format("decode aes XML ~p~n",[XmlElt]),
	Content = XmlElt#xmlElement.content,
	Map =
		lists:foldl(fun
							(El, Acc) when erlang:is_record(El,xmlElement)->
									Key = El#xmlElement.name,
									[ElContent] = El#xmlElement.content,
									Value = ElContent#xmlText.value,
									Acc#{Key => Value};
							(_El,Acc)->Acc
		end,#{}, Content),
	case maps:get(handler,State,undefined) of 
		undefined-> 
          Req1 = cowboy_req:reply(200, 
                                  #{<<"content-type">> => <<"text/plain">>}, 
                                  <<"">>, Req0),
          {ok,Req1,State};
		Handler -> 
			Reply = Handler:handle(Map),
			Req1 = cowboy_req:reply(200,
				#{<<"content-type">> => <<"application/xml">>},Reply,Req0),
			{ok,Req1,State}
	end;
	
handle(get,Req, State) ->
    QS = cowboy_req:parse_qs(Req),
    EcohStr = proplists:get_value(<<"echostr">>,QS),
    Req0 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>}, 
                            EcohStr, Req),
    {ok, Req0, State}.


decode(Req,State)->
		{ok,Body,Req0} = read_body(Req),
		{ok,XmlBody} =
			case maps:get(encrypt,State,true) of
				false ->  ai_string:to_iolist(Body);
				true  -> 
						QS = cowboy_req:parse_qs(Req0),
						Signature = proplists:get_value(<<"msg_signature">>,QS),
						Timestamp = proplists:get_value(<<"timestamp">>,QS),
						Nonce = proplists:get_value(<<"nonce">>,QS),
						decrypt(Signature,Timestamp,Nonce,Body)
		end,
		{XmlElt, _} = xmerl_scan:string(XmlBody),
		{XmlElt,Req0}.
decrypt(Signature,Timestamp,Nonce, Body)->
		[_H,P0] = binary:split(Body,<<"<Encrypt><![CDATA[">>),
		[P1,_T] = binary:split(P0,<<"]]></Encrypt>">>),
		XmlBody = ai_string:to_iolist(P1),

		Verify = ai_wx_signature:verify(Signature,Timestamp,Nonce,XmlBody),
		if 
				Verify == true -> decrypt(base64:decode(P1));
				true ->{error,not_verified}
		end.
decrypt(Body)->
		Base64Key = ai_wx_conf:app_key(),
		DecodeKey = base64:decode(Base64Key ++ "="),
		DecodeSize = erlang:byte_size(DecodeKey),
		{Key,IV} =
			if 
				DecodeSize > 16 -> 
					<<DecodeIV:16/binary,_Rest/bits>> = DecodeKey,
						{DecodeKey,DecodeIV};
				true -> {DecodeKey,DecodeKey}
			end,
		DecodeData = ai_wx_pkcs7:unpad(crypto:block_decrypt(aes_cbc,Key,IV,Body)),
		<<_Random:16/binary,XmlBodySize:32/big-unsigned-integer,Rest0/binary>> = DecodeData,
		<<XmlBody:XmlBodySize/binary,AppID/binary>> = Rest0,
		ConfAppID = ai_string:to_string(ai_wx_conf:app_id()),
		io:format("decode aes data ~ts AND id ~p~n",[XmlBody,AppID]),
		if 
				AppID == ConfAppID -> {ok,ai_string:to_iolist(XmlBody)};
				true -> {error,app_id}
		end.

read_body(Req)->
    read_body(Req,<<>>).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> {ok, << Acc/binary, Data/binary >>, Req0};
        {more, Data, Req0} -> read_body(Req0, << Acc/binary, Data/binary >>)
	end.

verify(QS)->
    Signature = proplists:get_value(<<"signature">>,QS),
    Timestamp = proplists:get_value(<<"timestamp">>,QS),
    Nonce = proplists:get_value(<<"nonce">>,QS),
    ai_wx_signature:verify(Signature, Timestamp, Nonce).
