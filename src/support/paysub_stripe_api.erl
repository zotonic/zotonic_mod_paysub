%% @copyright 2022-2024 Marc Worrell
%% @doc Stripe API for paid subscriptions and donations
%% @end

%% Copyright 2022-2024 Marc Worrrell
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

-module(paysub_stripe_api).

-export([
    fetch/4,

    flatten/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

-define(STRIPE_API_URL, "https://api.stripe.com/v1/").
-define(STRIPE_API_VERSION, <<"2025-09-30.clover">>).

fetch(Method, [ P | _ ] = Path, Payload, Context) when is_list(P); is_binary(P) ->
    Url = iolist_to_binary([ ?STRIPE_API_URL, lists:join($/, Path) ]),
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
                {content_type, "application/x-www-form-urlencoded"},
                {headers, [ {<<"Stripe-Version">>, ?STRIPE_API_VERSION} ]}
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
