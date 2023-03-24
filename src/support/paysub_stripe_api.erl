%% @copyright 2022-2023 Marc Worrell
%% @doc Stripe API for paid subscriptions and donations
%% @end

%% Copyright 2022-2023 Marc Worrrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


% TODO: extract enoent from this error:
% 2023-03-21 15:05:44 ERROR  site=sculpturenetwork  user_id=1 <0.20745.0> [paysub_stripe_api:fetch/4:80] ▸ url=https://api.stripe.com/v1/customers/cus_NYqYX2q1imNJn9 text="Stripe: error fetching data from API" result=error reason="{400,\"https://api.stripe.com/v1/customers/cus_NYqYX2q1imNJn9\",[{\"cache-control\",\"no-cache, no-store\"},{\"connection\",\"keep-alive\"},{\"date\",\"Tue, 21 Mar 2023 14:05:43 GMT\"},{\"server\",\"nginx\"},{\"content-length\",\"333\"},{\"content-type\",\"application/json\"},{\"access-control-allow-credentials\",\"true\"},{\"access-control-allow-methods\",\"GET, POST, HEAD, OPTIONS, DELETE\"},{\"access-control-allow-origin\",\"*\"},{\"access-control-expose-headers\",\"Request-Id, Stripe-Manage-Version, X-Stripe-External-Auth-Required, X-Stripe-Privileged-Session-Required\"},{\"access-control-max-age\",\"300\"},{\"idempotency-key\",\"30fee85f-cf5e-4037-a4fd-99451e7e80cf\"},{\"original-request\",\"req_VjZAbNug3CAt4P\"},{\"request-id\",\"req_VjZAbNug3CAt4P\"},{\"stripe-should-retry\",\"false\"},{\"stripe-version\",\"2019-09-09\"},{\"strict-transport-security\",\"max-age=63072000; includeSubDomains; preload\"}],0,<<\"{\\n  \\\"error\\\": {\\n    \\\"code\\\": \\\"resource_missing\\\",\\n    \\\"doc_url\\\": \\\"https://stripe.com/docs/error-codes/resource-missing\\\",\\n    \\\"message\\\": \\\"No such customer: 'cus_NYqYX2q1imNJn9'\\\",\\n    \\\"param\\\": \\\"id\\\",\\n    \\\"request_log_url\\\": \\\"https://dashboard.stripe.com/test/logs/req_VjZAbNug3CAt4P?t=1679407543\\\",\\n    \\\"type\\\": \\\"invalid_request_error\\\"\\n  }\\n}\\n\">>}" method=post in=zotonic_mod_paysub body="address%5Bcity%5D=&address%5Bcountry%5D=&address%5Bline1%5D=&address%5Bline2%5D=&address%5Bpostal_code%5D=&address%5Bstate%5D=&email=&metadata%5Badmin_url%5D=https%3A%2F%2Fcvc-sn.worrell.nl%3A8443%2Fen%2Fadmin%2Fedit%2F108374&metadata%5Bpage_url%5D=https%3A%2F%2Fcvc-sn.worrell.nl%3A8443%2Fen%2Fpage%2F108374%2Fjan-tester&metadata%5Brsc_id%5D=108374&name=Jan%20Tester&phone=&preferred_locales%5B0%5D=en"


-module(paysub_stripe_api).

-export([
    fetch/4,

    flatten/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(API_URL, "https://api.stripe.com/v1/").


fetch(Method, [ P | _ ] = Path, Payload, Context) when is_list(P); is_binary(P) ->
    Url = iolist_to_binary([ ?API_URL, lists:join($/, Path) ]),
    case m_config:get_value(mod_paysub, stripe_api_key, <<>>, Context) of
        <<>> ->
            ?LOG_WARNING(#{
                in => zotonic_mod_paysub,
                psp => stripe,
                text => <<"No Stripe API key configured in mod_paysub.stripe_api_key">>,
                result => error,
                reason => no_stripe_apikey,
                api_url => Url
            }),
            {error, no_stripe_apikey};
        ApiKey ->
            Options = [
                {authorization, <<"Bearer ", ApiKey/binary>>},
                {content_type, "application/x-www-form-urlencoded"}
            ],
            RequestBody = flatten(Payload),
            Result = case Method of
                post ->
                    z_fetch:fetch_json(Method, Url, RequestBody, Options, Context);
                put ->
                    z_fetch:fetch_json(Method, Url, RequestBody, Options, Context);
                get ->
                    z_fetch:fetch_json(Method, qarg(Url, RequestBody), <<>>, Options, Context);
                delete ->
                    z_fetch:fetch_json(Method, qarg(Url, RequestBody), <<>>, Options, Context)
            end,
            case Result of
                {ok, _} = Ok ->
                    Ok;
                {error, {400 = Code, _Url, _Hs, _Size, <<"{", _/binary>> = ReplyBody}} ->
                    JSON = jsx:decode(ReplyBody),
                    case JSON of
                        #{
                            <<"error">> := #{
                                <<"code">> := <<"resource_missing">>
                            }
                        } ->
                            ?LOG_INFO(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe: error fetching data from API">>,
                                result => error,
                                reason => enoent,
                                http_status => Code,
                                url => Url,
                                method => Method,
                                body => JSON
                            }),
                            {error, enoent};
                        _ ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe: error fetching data from API">>,
                                result => error,
                                reason => http_error(Code),
                                http_status => Code,
                                url => Url,
                                method => Method,
                                body => JSON
                            }),
                            {error, http_error(Code)}
                    end;
                {error,{404, _FinalUrl, _Hs, _Size, _ReplyBody}} ->
                    ?LOG_INFO(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe: error fetching data from API">>,
                        result => error,
                        reason => enoent,
                        http_status => 404,
                        url => Url,
                        method => Method
                    }),
                    {error, enoent};
                {error,{Code, _FinalUrl, _Hs, _Size, ReplyBody}} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe: error fetching data from API">>,
                        result => error,
                        reason => http_error(Code),
                        http_status => Code,
                        url => Url,
                        method => Method,
                        body => ReplyBody
                    }),
                    {error, http_error(Code)};
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe: error fetching data from API">>,
                        result => error,
                        reason => Reason,
                        url => Url,
                        method => Method
                    }),
                    {error, Reason}
            end
    end.

http_error(404) -> enoent;
http_error(403) -> eacces;
http_error(401) -> eacces;
http_error(Code) -> Code.

qarg(Url, <<>>) -> Url;
qarg(Url, Qs) -> <<Url/binary, $?, Qs/binary>>.


%% @doc Flatten a nested map or list to an application/x-www-form-urlencoded string.
-spec flatten(Data) -> Body when
    Data :: undefined | map() | list() | integer() | binary() | boolean(),
    Body :: binary().
flatten(undefined) ->
    <<>>;
flatten(Data) ->
    Ks = flatten_1(<<>>, Data, []),
    Pairs = lists:map(
        fun({K, V}) ->
            K1 = z_url:url_encode(K),
            V1 = z_url:url_encode(V),
            <<K1/binary, $=, V1/binary>>
        end,
        Ks),
    iolist_to_binary(lists:join($&, lists:reverse(Pairs))).

flatten_1(KeyPrefix, Map, FlattenAcc) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = key(KeyPrefix, K),
            flatten_1(K1, V, Acc)
        end,
        FlattenAcc,
        Map);
flatten_1(KeyPrefix, List, FlattenAcc) when is_list(List) ->
    {_, FAcc1} = lists:foldl(
        fun(V, {N, Acc}) ->
            K1 = key(KeyPrefix, N),
            Acc1 = flatten_1(K1, V, Acc),
            {N+1, Acc1}
        end,
        {0, FlattenAcc},
        List),
    FAcc1;
flatten_1(Key, V, Acc) ->
    [ {Key, z_convert:to_binary(V)} | Acc ].


key(<<>>, K) ->
    z_convert:to_binary(K);
key(Prefix, K) ->
    z_convert:to_binary([ Prefix, $[,  z_convert:to_binary(K), $] ]).
