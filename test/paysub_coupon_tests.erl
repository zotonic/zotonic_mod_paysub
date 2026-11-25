%% @copyright 2026 Marc Worrell
%% @doc Stripe coupon evidence, free checkout and synchronization regressions.
-module(paysub_coupon_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-export([session/1, invoice/1]).

checkout_test() ->
    meck:new(m_paysub, [passthrough, no_link]),
    try
        lists:foreach(fun(Discount) ->
            S = session(Discount),
            meck:expect(m_paysub, checkout_update, fun(stripe, <<"coupon">>, U, _) ->
                ?assertEqual(5000 - Discount, maps:get(amount, U)),
                ?assertEqual(maps:with([<<"amount_subtotal">>, <<"amount_total">>,
                    <<"total_details">>, <<"discounts">>], S), maps:get(discount_details, U)), ok
            end),
            meck:expect(m_paysub, checkout_completed, fun(stripe, <<"coupon">>, _) -> ok end),
            ?assertEqual(ok, paysub_stripe:checkout_session_completed(S, #context{})),
            % The asynchronous/API sync path also attempts the guarded notification.
            ?assertEqual(ok, paysub_stripe:checkout_session_sync(S, #context{}))
        end, [2500, 5000]),
        meck:reset(m_paysub),
        meck:expect(m_paysub, checkout_update, fun(_, _, _, _) -> {error, enoent} end),
        ?assertEqual({error, enoent}, paysub_stripe:checkout_session_completed(session(5000), #context{})),
        ?assertNot(meck:called(m_paysub, checkout_completed, '_'))
    after meck:unload(m_paysub) end.

invoice_test() ->
    meck:new(m_paysub, [passthrough, no_link]),
    try
        lists:foreach(fun(Discount) ->
            I = invoice(Discount),
            meck:expect(m_paysub, sync_invoice, fun(stripe, Normalized, _) ->
                ?assertEqual(5000 - Discount, maps:get(total, Normalized)),
                Details = maps:get(discount_details, Normalized),
                ?assertEqual(5000, maps:get(<<"subtotal">>, Details)),
                ?assertEqual(maps:get(<<"total_discount_amounts">>, I), maps:get(<<"total_discount_amounts">>, Details)),
                ?assertEqual(maps:get(<<"discounts">>, I), maps:get(<<"discounts">>, Details)),
                [Line] = maps:get(items, Normalized),
                ?assertEqual([#{<<"amount">> => Discount}], maps:get(<<"discount_amounts">>, maps:get(discount_details, Line))),
                ok
            end),
            ?assertEqual(ok, paysub_stripe:sync_invoice(I, #context{}))
        end, [2500, 5000])
    after meck:unload(m_paysub) end.

session(Discount) ->
    #{<<"id">> => <<"cs_coupon">>, <<"mode">> => <<"payment">>,
      <<"customer">> => undefined, <<"status">> => <<"complete">>,
      <<"payment_intent">> => undefined, <<"payment_status">> =>
        case Discount of 5000 -> <<"no_payment_required">>; _ -> <<"paid">> end,
      <<"client_reference_id">> => <<"coupon">>, <<"currency">> => <<"eur">>,
      <<"amount_total">> => 5000 - Discount, <<"amount_subtotal">> => 5000,
      <<"total_details">> => #{<<"amount_discount">> => Discount},
      <<"discounts">> => [#{<<"coupon">> => <<"coupon_test">>, <<"promotion_code">> => <<"promo_test">>}]}.

invoice(Discount) ->
    #{<<"id">> => <<"in_coupon">>, <<"object">> => <<"invoice">>,
      <<"currency">> => <<"eur">>, <<"amount_due">> => 5000 - Discount,
      <<"amount_paid">> => 5000 - Discount, <<"amount_remaining">> => 0,
      <<"total">> => 5000 - Discount, <<"subtotal">> => 5000,
      <<"discounts">> => [#{<<"id">> => <<"di_coupon">>, <<"source">> => #{<<"coupon">> => <<"coupon_test">>}}],
      <<"total_discount_amounts">> => [#{<<"amount">> => Discount, <<"discount">> => <<"di_coupon">>}],
      <<"collection_method">> => <<"charge_automatically">>, <<"default_payment_method">> => undefined,
      <<"attempted">> => true, <<"status">> => <<"paid">>, <<"created">> => 1795082400,
      <<"period_start">> => 1795082400, <<"period_end">> => 1795082400,
      <<"customer">> => <<"cus_coupon">>, <<"customer_email">> => undefined, <<"customer_name">> => undefined,
      <<"lines">> => #{<<"object">> => <<"list">>, <<"url">> => <<"/v1/invoices/in_coupon/lines">>, <<"has_more">> => false,
          <<"data">> => [#{<<"description">> => <<"Full price">>, <<"currency">> => <<"eur">>,
              <<"amount">> => 5000, <<"proration">> => false,
              <<"discount_amounts">> => [#{<<"amount">> => Discount}]}]}}.
