-module(ttalk_wechat_http).
-export([do_get/1,do_post/2]).

-spec do_get(URL::binary()) -> binary().
do_get(URL)->
    Method = get,
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, Headers),
    {ok, Body} = hackney:body(ClientRef),
    Body.

-spec do_post(URL::binary(),Body::binary()) -> binary().
do_post(URL, Body) ->
    Method = post,
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, ClientRef} = hackney:request(Method, URL, ReqHeaders, stream, []),
    ok  = hackney:send_body(ClientRef, Body),
    {ok, _Status, _Headers, ClientRef} = hackney:start_response(ClientRef),
    {ok, Body} = hackney:body(ClientRef),
    Body.
