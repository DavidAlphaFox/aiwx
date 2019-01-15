-module (ai_wx_pkcs7).

-export ([pad/1]).
-export ([unpad/1]).

pad(Bin) ->
    Diff = erlang:byte_size(Bin) rem 32,
    PadDiff = 32 - Diff,
    Pad = lists:duplicate(PadDiff,PadDiff),
    PadBin = erlang:list_to_binary(Pad),
    <<Bin/binary,PadBin/binary>>.

unpad(<<>>) -><<>>;
unpad(Bin) ->
    Last = binary:last(Bin),
    Size = erlang:byte_size(Bin) - Last,
    <<RealData:Size/binary,_Padding/binary>> = Bin,
    RealData.
