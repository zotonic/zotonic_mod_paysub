%% @copyright 2026 Marc Worrell
%% @doc Ensure requests do not depend on the Stripe account's API version.
-module(paysub_stripe_api_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").

api_version_test() ->
    meck:new([m_config, z_fetch], [passthrough, no_link]),
    try
        meck:expect(m_config, get_value,
            fun(mod_paysub, stripe_api_key, <<>>, _) -> <<"test">> end),
        meck:expect(z_fetch, fetch_json, fun(get, _, <<>>, Options, _) ->
            ?assertEqual(<<"2025-09-30.clover">>,
                proplists:get_value(<<"Stripe-Version">>, proplists:get_value(headers, Options))),
            {ok, #{}}
        end),
        ?assertEqual({ok, #{}}, paysub_stripe_api:fetch(
            get, [<<"checkout">>, <<"sessions">>], undefined, #context{}))
    after
        meck:unload([m_config, z_fetch])
    end.
