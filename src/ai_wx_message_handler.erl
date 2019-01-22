-module(ai_wx_message_handler).
-include_lib("xmerl/include/xmerl.hrl").

-export([init/2]).

-callback init(term(),term()) -> {ok,term(),term()}.
-callback handle_message(string(),maps:maps(),term()) -> binary().
-callback handle_event(string(),maps:maps(),term())-> binary().


init(Req,State)->
	case maps:get(handler,State,undefined) of 
		undefined -> 
			Req0 = cowboy_req:reply(200, 
				#{<<"content-type">> => <<"text/plain">>}, 
				<<"">>, Req),
			{ok,Req0,State};
		Handler -> 
			{ok,Req0,Context} = Handler:init(Req,State),
			handle(Handler,Req0,State#{context => Context})
	end.

handle(Handler,Req,State)->
	QS = cowboy_req:parse_qs(Req),
	Verify = verify(QS,State),
	if
		Verify == false -> 
			Req0 = cowboy_req:reply(200, 
				#{<<"content-type">> => <<"text/plain">>},
				<<"">>, Req),
			{ok,Req0,State};
		true ->
			case cowboy_req:method(Req) of 
				<<"GET">> -> handle(get,Handler,Req,State);
				<<"POST">> -> handle(post,Handler,Req,State);
				_ -> 
					Req0 = cowboy_req:reply(200, 
						#{<<"content-type">> => <<"text/plain">>},
						<<"">>, Req),
					{ok,Req0,State}
			end
	end.

handle(post,Handler,Req,State)-> 
	{XmlElt,Req0} = decode(Req,State),
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
	Req1 =
		case process(Handler,Map,State) of 
			error -> 
				cowboy_req:reply(200, 
					#{<<"content-type">> => <<"text/plain">>}, <<"">>, Req0);
			Reply ->
				Reply0 = encode(Reply,Req0,State), 
				cowboy_req:reply(200,
					#{<<"content-type">> => <<"application/xml">>},Reply0,Req0)
		end,
	{ok,Req1,State};

handle(get,_Handler,Req, State) ->
    QS = cowboy_req:parse_qs(Req),
    EcohStr = proplists:get_value(<<"echostr">>,QS),
    Req0 = cowboy_req:reply(200,
            #{<<"content-type">> => <<"text/plain">>}, 
            EcohStr, Req),
    {ok, Req0, State}.


process(Handler,Map,State)->
	MsgType = maps:get('MsgType',Map),
	Context = maps:get(context,State),
	try 
		if 
			MsgType == "event" -> 
				Key = maps:get('Event',Map),
				Handler:handle_event(Key,Map,Context);
			true -> Handler:handle_message(MsgType,Map,Context)
		end
	catch 
		_Reason:_Error -> error
	end.

verify(QS,State)->
	Context = maps:get(context,State),
    Signature = proplists:get_value(<<"signature">>,QS),
    Timestamp = proplists:get_value(<<"timestamp">>,QS),
    Nonce = proplists:get_value(<<"nonce">>,QS),
    ai_wx_signature:verify(Signature, Timestamp, Nonce,Context).


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
						decrypt(Signature,Timestamp,Nonce,Body,State)
			end,
		{XmlElt, _} = xmerl_scan:string(XmlBody),
		{XmlElt,Req0}.

read_body(Req)-> read_body(Req,<<>>).

read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req0} -> {ok, << Acc/binary, Data/binary >>, Req0};
        {more, Data, Req0} -> read_body(Req0, << Acc/binary, Data/binary >>)
	end.

decrypt(Signature,Timestamp,Nonce, Body,State)->
		Context = maps:get(context,State),

		[_H,P0] = binary:split(Body,<<"<Encrypt><![CDATA[">>),
		[P1,_T] = binary:split(P0,<<"]]></Encrypt>">>),
		CryptedXmlBody = ai_string:to_iolist(P1),
		Verify = ai_wx_signature:verify(Signature,Timestamp,Nonce,CryptedXmlBody,Context),
		if 
			Verify == true -> 
				{Key,IV} = app_key(Context),
				DecodeData = ai_wx_pkcs7:unpad(crypto:block_decrypt(aes_cbc,Key,IV,CryptedXmlBody)),
				<<_Random:16/binary,XmlBodySize:32/big-unsigned-integer,Rest0/binary>> = DecodeData,
				<<XmlBody:XmlBodySize/binary,AppID/binary>> = Rest0,
				ConfAppID = ai_string:to_string(ai_wx_conf:app_id(Context)),
				if 
					AppID == ConfAppID -> {ok,ai_string:to_iolist(XmlBody)};
					true -> {error,app_id}
				end;
			true ->{error,not_verified}
		end.

app_key(Context)->
		Base64Key = ai_wx_conf:app_key(Context),
		DecodeKey = base64:decode(Base64Key ++ "="),
		DecodeSize = erlang:byte_size(DecodeKey),
		if
			DecodeSize > 16 -> 
				<<DecodeIV:16/binary,_Rest/bits>> = DecodeKey,
				{DecodeKey,DecodeIV};
			true -> {DecodeKey,DecodeKey}
		end.

encode(Reply,Req,State)->
	case maps:get(encrypt,State,true) of
			false ->  Reply;
			true  -> 
				QS = cowboy_req:parse_qs(Req),
				Timestamp = proplists:get_value(<<"timestamp">>,QS),
				Nonce = proplists:get_value(<<"nonce">>,QS),
				encrypt(Timestamp,Nonce,Reply,State)
	end.
encrypt(Timestamp,Nonce,Reply,State)->
		Context = maps:get(context,State),
		XmlBodySize = erlang:byte_size(Reply),
		Random = ai_string:to_string(ai_ascii_random:rand(16)),
		AppID = ai_string:to_string(ai_wx_conf:app_id(Context)),
		Msg = <<Random/binary,XmlBodySize:32/big-unsigned-integer,Reply/binary,AppID/binary>>,
		Msg0 = ai_wx_pkcs7:pad(Msg),
		{Key,IV} = app_key(Context),
		Msg1 = crypto:block_encrypt(aes_cbc,Key,IV,Msg0),
		Msg2 = base64:encode(Msg1),
		Signature = ai_wx_signature:sign_message(Timestamp,Nonce,Msg2,State),
		ai_wx_xml:encrypted_message(Signature,Timestamp,Nonce,Msg2).
