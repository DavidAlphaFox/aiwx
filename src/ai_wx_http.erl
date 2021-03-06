-module(ai_wx_http).

-export([get/2,post/3,post_multipart/3]).

get(HOST,Path)->
    {ok, ConnPid} = gun:open(HOST,443, #{transport => tls}),
    {ok, _Protocol} = gun:await_up(ConnPid),

    StreamRef = gun:get(ConnPid,Path,[
        {<<"Content-Type">>, <<"application/json">>},
        {<<"accept">>, "application/json"},
        {<<"user-agent">>, "ai_wx/0.1.0"}
    ]),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} -> {Status,Headers};
        {response, nofin, Status, Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            {Status,Headers,jsx:decode(Body)}
    end.
post(HOST,Path, ReqBody) ->
    {ok, ConnPid} = gun:open(HOST,443, #{transport => tls}),
    {ok, _Protocol} = gun:await_up(ConnPid),

    StreamRef = gun:post(ConnPid,Path,[
        {<<"Content-Type">>, <<"application/json">>},
        {<<"accept">>, "application/json"},
        {<<"user-agent">>, "ai_wx/0.1.0"}
    ],ReqBody),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} -> {Status,Headers};
        {response, nofin, Status, Headers} ->
            {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
            {Status,Headers,jsx:decode(ResBody)}
    end.
post_multipart(HOST,Path,File)->
    {ok, ConnPid} = gun:open(HOST,443, #{transport => tls}),
    {ok, _Protocol} = gun:await_up(ConnPid),
    {Boundary,EncodeForm,Length} =  ai_multipart:encode([{file,<<"media">>,File,[]}]),
    StreamRef = gun:post(ConnPid,Path,[
        {<<"Content-Type">>, <<"multipart/form-data; boundary=",Boundary/binary>>},
        {<<"Content-Length">>,ai_string:to_string(Length)},
        {<<"accept">>, "application/json"},
        {<<"user-agent">>, "ai_wx/0.1.0"}
    ],EncodeForm),
    case gun:await(ConnPid, StreamRef) of
        {response, fin, Status, Headers} -> {Status,Headers};
        {response, nofin, Status, Headers} ->
            {ok, ResBody} = gun:await_body(ConnPid, StreamRef),
            {Status,Headers,jsx:decode(ResBody)}
    end.