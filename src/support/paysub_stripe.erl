%% @copyright 2022 Marc Worrell
%% @doc Stripe support for payments and subscriptions.
%% @end

%% Copyright 2022 Marc Worrrell
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


% https://stripe.com/docs/billing/subscriptions/build-subscriptions
%
% 1. Checkout
% -----------
% https://stripe.com/docs/checkout/quickstart
% 1.1 - Start checkout session with the correct price
% 1.2 - Redirect customer
% 1.3 - Redirect to success_url
% 1.4 - Wait for webhook:
%
% 2. Customer portal
% ------------------
% https://stripe.com/docs/customer-management
% https://stripe.com/docs/customer-management/integrate-customer-portal


-module(paysub_stripe).

-export([
    event/2,
    checkout_session_create/2,
    checkout_session_sync/2,

    sync_products/1,
    sync_prices/1,

    sync_customers/1,
    sync_customer/2,
    delete_customer/2,

    ensure_stripe_customer/2,

    sync_subscriptions/1,
    sync_subscription/2,
    delete_subscription/2,

    sync_invoices/1,
    sync_invoice/2,
    delete_invoice/2,

    schedule_sync/2,
    async_task/2
    ]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc UI - checkout redirect
event(#submit{ message = {checkout, Args} }, Context) ->
    case checkout_session_create(Args, Context) of
        {ok, Data} ->
            ?DEBUG(Data),
            Context;
        {error, _} ->
            Context
    end.

%% @doc Start a Stripe checkout for the given payment/subscription request.
checkout_session_create(Args, Context) ->
    UserId = z_acl:user(Context),
    Mode = case proplists:get_value(mode, Args) of
        undefined -> subscription;
        <<"subscription">> -> subscription;
        <<"payment">> -> payment;
        subscription -> subscription;
        payment -> payment
    end,
    {ok, CheckoutNr} = m_paysub:checkout_create(stripe, UserId, Mode, Args, Context),
    DoneUrl0 = z_dispatcher:url_for(paysub_psp_done, [ {checkout_nr, CheckoutNr} ], Context),
    DoneUrl = z_context:abs_url(DoneUrl0, Context),
    Payload = #{
        cancel_url => DoneUrl,
        success_url => DoneUrl,
        mode => Mode,
        client_reference_id => CheckoutNr,
        metadata => #{
            rsc_id => UserId
        },
        line_items => line_items(Mode, Args, Context)
    },
    Payload1 = case m_paysub:get_customer(stripe, UserId, Context) of
        {ok, #{
            <<"psp_customer_id">> := CustId
        }} ->
            Payload#{
                customer => CustId,
                customer_update => #{
                    address => true
                }
            };
        {error, enoent} when UserId =/= undefined ->
            PE1 = case m_rsc:p_no_acl(UserId, email_raw, Context) of
                undefined ->
                    Payload;
                Email ->
                    Payload#{
                        customer_email => Email
                    }
            end,
            PE2 = PE1#{
                consent_collection => #{
                    terms_of_service => required
                }
            },
            case Mode of
                subscription ->
                    PE2#{
                        subscription_data => #{
                            metadata => #{
                                rsc_id => UserId
                            }
                        }
                    };
                payment ->
                    PE2#{
                        invoice_creation => #{
                            enabled => true
                        }
                    }
            end;
        {error, enoent} when UserId =:= undefined, Mode =:= payment ->
            Payload;
        {error, enoent} when UserId =:= undefined, Mode =:= subscription ->
            Payload#{
                consent_collection => #{
                    terms_of_service => required
                },
                subscription_data => #{
                    metadata => #{
                        rsc_id => UserId
                    }
                }
            }
    end,
    case paysub_stripe_api:fetch(post, [ "checkout", "sessions" ], Payload1, Context) of
        {ok, #{
            <<"url">> := Url,
            <<"id">> := PspCheckoutId,
            <<"status">> := Status,
            <<"payment_status">> := PaymentStatus,
            <<"currency">> := Currency,
            <<"amount_total">> := Amount
        }} ->
            Update = #{
                psp_checkout_id => PspCheckoutId,
                status => Status,
                payment_status => PaymentStatus,
                currency => z_string:to_upper(Currency),
                amount => Amount
            },
            m_paysub:checkout_update(stripe, CheckoutNr, Update, Context),
            {ok, Url};
        {error, _} = Error ->
            Error
    end.


line_items(payment, Args, Context) ->
    % TODO: figure out line items for individual payments
    [];
line_items(subscription, Args, _Context) ->
    {price, Price} = proplists:lookup(price, Args),
    [
        #{
            price => Price,
            quantity => 1
        }
    ].


%% @doc Called by the controller_subscription_stripe_redirect to handle incoming
%% webhook events.
checkout_session_sync(Session, Context) ->
    #{
        <<"id">> := _CheckoutId,
        <<"customer">> := CustomerId,
        <<"status">> := Status,
        <<"payment_status">> := PaymentStatus,
        <<"client_reference_id">> := CheckoutNr,
        <<"currency">> := Currency,
        <<"amount_total">> := Amount
    } = Session,
    Update = #{
        psp_customer_id => CustomerId,
        status => Status,
        payment_status => PaymentStatus,
        currency => z_string:to_upper(Currency),
        amount => Amount
    },
    m_paysub:checkout_update(stripe, CheckoutNr, Update, Context),
    ok.


%% @doc Sync all products in Stripe with products here.
-spec sync_products(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_products(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"products">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Prods} ->
            Prods1 = lists:map(fun(P) -> stripe_prod(P, Context) end, Prods),
            ok = m_paysub:sync_products(stripe, Prods1, Context),
            ?LOG_INFO(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: sync products">>,
                result => ok,
                psp => stripe,
                product_count => length(Prods1)
            }),
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: sync products failed">>,
                psp => stripe,
                result => error,
                reason => Reason
            }),
            Error
    end.

stripe_prod(#{
        <<"id">> := Id,
        <<"object">> := <<"product">>,
        <<"active">> := IsActive,
        <<"default_price">> := DefaultPrice,
        <<"description">> := Description,
        <<"name">> := Name
    } = StripeProd, Context) ->
    Prod = #{
        psp => <<"stripe">>,
        psp_product_id => Id,
        psp_default_price_id => DefaultPrice,
        is_active => IsActive,
        name => Name,
        description => Description
    },
    case maps:get(<<"metadata">>, StripeProd, #{}) of
        #{ <<"user_group_id">> := UG } ->
            case m_rsc:rid(UG, Context) of
                undefined ->
                    Prod;
                UGId ->
                    Prod#{ user_group_id => UGId }
            end;
        _ ->
            Prod
    end.


%% @doc Sync all prices in Stripe with prices here.
-spec sync_prices(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_prices(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"prices">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Prices} ->
            Prices1 = lists:map(fun stripe_price/1, Prices),
            ok = m_paysub:sync_prices(stripe, Prices1, Context),
            ?LOG_INFO(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: sync prices">>,
                result => ok,
                psp => stripe,
                product_count => length(Prices1)
            }),
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: sync prices failed">>,
                psp => stripe,
                result => error,
                reason => Reason
            }),
            Error
    end.

stripe_price(#{
        <<"id">> := Id,
        <<"object">> := <<"price">>,
        <<"active">> := IsActive,
        <<"nickname">> := Nickname,
        <<"product">> := ProductId,
        <<"currency">> := Currency,
        <<"unit_amount">> := UnitAmount,
        <<"type">> := Type
    } = P) ->
    #{
        psp => <<"stripe">>,
        psp_price_id => Id,
        psp_product_id => ProductId,
        is_active => IsActive,
        is_recurring => Type =:= <<"recurring">>,
        recurring_period => recurring_period(P),
        name => z_convert:to_binary(Nickname),
        currency => z_string:to_upper(Currency),
        amount => UnitAmount
    }.

recurring_period(#{ <<"recurring">> := #{ <<"interval">> := Interval }}) ->
    Interval;
recurring_period(#{ <<"recurring">> := undefined }) ->
    undefined.


schedule_sync(What, Context) ->
    z_pivot_rsc:insert_task(?MODULE, async_task, What, [What], Context).

async_task(prices, Context) ->
    sync_prices(Context);
async_task(products, Context) ->
    sync_products(Context).




%% @doc Sync a single customer after a webhook event.
-spec sync_customer(Cust, Context) -> ok when
    Cust :: map(),
    Context :: z:context().
sync_customer(Cust, Context) ->
    SCust = stripe_customer(Cust, Context),
    m_paysub:sync_customer(stripe, SCust, Context).

%% @doc Sync all customers in Stripe with customers here.
-spec sync_customers(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_customers(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"customers">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Custs} ->
            Custs1 = lists:map(fun(C) -> stripe_customer(C, Context) end, Custs),
            m_paysub:sync_customers(stripe, Custs1, Context);
        {error, _} = Error ->
            Error
    end.

stripe_customer(#{
        <<"id">> := Id,
        <<"object">> := <<"customer">>,
        <<"email">> := Email,
        <<"name">> := Name
    } = Cust, Context) ->
    P = #{
        psp => <<"stripe">>,
        psp_customer_id => Id,
        name => z_convert:to_binary(Name),
        email => Email,
        pref_language => pref_lang(Cust, Context),
        address_country => adr_country(Cust)
    },
    case maybe_user_id(Cust) of
        undefined ->
            P;
        UserId ->
            UserId1 = case m_rsc:exists(UserId, Context) of
                true -> UserId;
                false -> undefined
            end,
            P#{
                rsc_id => UserId1
            }
    end.


%% @doc Delete a single customer after a webhook event.
-spec delete_customer(Cust, Context) -> ok | {error, enoent} when
    Cust :: map(),
    Context :: z:context().
delete_customer(#{ <<"id">> := Id }, Context) ->
    m_paysub:delete_customer(stripe, Id, Context).


%% @doc Ensure that a stripe customer is created for the given user-id
-spec ensure_stripe_customer(UserId, Context) -> {ok, StripeId} | {error, Reason} when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    StripeId :: binary(),
    Reason :: term().
ensure_stripe_customer(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            {error, enoent};
        RscId ->
            case m_paysub:get_customer(stripe, RscId, Context) of
                {ok, #{
                    <<"psp_customer_id">> := PSPCustId
                }} ->
                    {ok, PSPCustId};
                {error, enoent} ->
                    ContextSudo = z_acl:sudo(Context),
                    {Name, _} = z_template:render_to_iolist(<<"_name.tpl">>, [ {id, RscId} ], ContextSudo),
                    AdminUrl = z_dispatcher:url_for(admin_edit_rsc, [ {id, RscId} ], Context),
                    Cust = #{
                        <<"address">> => billing_address(RscId, ContextSudo),
                        <<"email">> => billing_email(RscId, ContextSudo),
                        <<"phone">> => m_rsc:p_no_acl(RscId, <<"phone">>, ContextSudo),
                        <<"name">> => z_string:trim(Name),
                        <<"preferred_locales">> => [ bin(pref_language(RscId, ContextSudo)) ],
                        <<"metadata">> => #{
                            <<"rsc_id">> => RscId,
                            <<"page_url">> => m_rsc:p_no_acl(RscId, page_url_abs, ContextSudo),
                            <<"admin_url">> => z_context:abs_url(AdminUrl, Context)
                        }
                    },
                    case paysub_stripe_api:fetch(post, [ <<"customers">> ], Cust, Context) of
                        {ok, #{ <<"id">> := PSPCustId } = CustObject} ->
                            sync_customer(CustObject, Context),
                            ?LOG_INFO(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe created customer for resource">>,
                                result => ok,
                                rsc_id => RscId,
                                psp_customer_id => PSPCustId,
                                psp => stripe
                            }),
                            {ok, PSPCustId};
                        {error, Reason} = Error ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe error creating customer for resource">>,
                                rsc_id => RscId,
                                result => error,
                                reason => Reason,
                                psp => stripe
                            }),
                            Error
                    end
            end
    end.

billing_address(Id, Context) ->
    BillingCountry = m_rsc:p(Id, <<"billing_country">>, Context),
    case z_utils:is_empty(BillingCountry) of
        true ->
            #{
                <<"city">> => bin(m_rsc:p(Id, <<"address_city">>, Context)),
                <<"country">> => z_string:to_upper(bin(m_rsc:p(Id, <<"address_country">>, Context))),
                <<"line1">> => bin(m_rsc:p(Id, <<"address_street_1">>, Context)),
                <<"line2">> => bin(m_rsc:p(Id, <<"address_street_2">>, Context)),
                <<"postal_code">> => bin(m_rsc:p(Id, <<"address_postcode">>, Context)),
                <<"state">> => bin(m_rsc:p(Id, <<"address_state">>, Context))
            };
        false ->
            #{
                <<"city">> => bin(m_rsc:p(Id, <<"billing_city">>, Context)),
                <<"country">> => z_string:to_upper(bin(BillingCountry)),
                <<"line1">> => bin(m_rsc:p(Id, <<"billing_street_1">>, Context)),
                <<"line2">> => bin(m_rsc:p(Id, <<"billing_street_2">>, Context)),
                <<"postal_code">> => bin(m_rsc:p(Id, <<"billing_postcode">>, Context)),
                <<"state">> => bin(m_rsc:p(Id, <<"billing_state">>, Context))
            }
    end.

billing_email(Id, Context) ->
    BillingEmail = m_rsc:p(Id, <<"billing_email">>, Context),
    case z_utils:is_empty(BillingEmail) of
        true -> m_rsc:p(Id, <<"email">>, Context);
        false -> BillingEmail
    end.

pref_language(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"pref_language">>, Context) of
        undefined ->
            z_context:language(Context);
        Lang ->
            Lang
    end.

bin(V) ->
    z_convert:to_binary(V).


%% @doc Update a single subscription after a webhook event. Optionally
%% copy the rsc_id from the sub metadata to the customer metadata.
-spec sync_subscription(Sub, Context) -> ok | {error, enoent} when
    Sub :: map(),
    Context :: z:context().
sync_subscription(Sub, Context) ->
    % TODO:
    % - Optional rsc_id from metadata -> check with current customer
    SSub = stripe_subscription(Sub, Context),
    PriceIds = stripe_subscription_prices(Sub),
    ok = m_paysub:sync_subscription(stripe, SSub, PriceIds, Context),
    OptRscId = maps:get(rsc_id, SSub, undefined),
    CustomerId = maps:get(psp_customer_id, SSub),
    maybe_set_customer_rsc_id(OptRscId, CustomerId, Context).

stripe_subscription(#{
        <<"id">> := Id,
        <<"customer">> := CustomerId,
        <<"status">> := Status,
        <<"canceled_at">> := CanceledAt,
        <<"cancel_at">> := CancelAt,
        <<"current_period_start">> := CurrPeriodStart,
        <<"current_period_end">> := CurrPeriodEnd,
        <<"trial_start">> := TrialStart,
        <<"trial_end">> := TrialEnd
    } = Sub, Context) ->
    PspSub = #{
        psp => <<"stripe">>,
        psp_subscription_id => Id,
        psp_customer_id => CustomerId,
        status => Status,
        period_start => timestamp_to_datetime(CurrPeriodStart),
        period_end => timestamp_to_datetime(CurrPeriodEnd),
        trial_start => timestamp_to_datetime(TrialStart),
        trial_end => timestamp_to_datetime(TrialEnd),
        canceled_at => timestamp_to_datetime(CanceledAt),
        cancel_at => timestamp_to_datetime(CancelAt)
    },
    case m_rsc:rid(maybe_user_id(Sub), Context) of
        undefined ->
            PspSub;
        RscId ->
            PspSub#{
                rsc_id => RscId
            }
    end.

maybe_set_customer_rsc_id(undefined, _CustId, _Context) ->
    ok;
maybe_set_customer_rsc_id(_RscId, undefined, _Context) ->
    ok;
maybe_set_customer_rsc_id(RscId, CustId, Context) ->
    case m_paysub:get_customer(stripe, CustId, Context) of
        {ok, #{ <<"rsc_id">> := undefined }}->
            % 1. Update our local customer id
            m_paysub:update_customer_rsc_id(stripe, CustId, RscId, Context),
            % 2. Update the customer at Stripe
            Payload = #{
                <<"metadata">> => #{
                    <<"rsc_id">> => RscId
                }
            },
            Path = [ <<"customers">>, CustId ],
            paysub_stripe_api:fetch(post, Path, Payload, Context),
            ok;
        {ok, #{ <<"rsc_id">> := RscId }} ->
            ok;
        {ok, #{ <<"rsc_id">> := OtherRscId }} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe non matching customer rsc id on customer and subscription">>,
                result => error,
                reason => rsc_id_mismatch,
                subscription_rsc_id => RscId,
                customer_rsc_id => OtherRscId,
                psp => stripe,
                psp_customer_id => CustId
            }),
            ok;
        {error, enoent} ->
            ok
    end.

stripe_subscription_prices(
    #{
        <<"items">> := #{
            <<"data">> := Items
        }
    }) ->
    lists:map(
        fun(#{
            <<"id">> := _Id,
            <<"object">> := <<"subscription_item">>,
            <<"price">> := #{
                <<"id">> := PriceId
            }
        }) ->
            PriceId
        end,
        Items).

%% @doc Fetch all subscriptions from Stripe, add them to the local database.
-spec sync_subscriptions(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_subscriptions(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"subscriptions">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Subs} ->
            lists:foreach(
                fun(Sub) ->
                    sync_subscription(Sub, Context)
                end,
                Subs);
        {error, _} = Error ->
            Error
    end.

%% @doc Delete a single subscription after a webhook event.
-spec delete_subscription(Sub, Context) -> ok | {error, enoent} when
    Sub :: map(),
    Context :: z:context().
delete_subscription(#{ <<"id">> := Id }, Context) ->
    m_paysub:delete_subscription(stripe, Id, Context).


%% @doc Sync all invoices in Stripe with invoices here.
-spec sync_invoices(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_invoices(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"invoices">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Invs} ->
            Invs1 = lists:map(
                fun(Inv) ->
                    stripe_invoice(Inv, Context)
                end, Invs),
            m_paysub:sync_invoices(stripe, Invs1, Context);
        {error, _} = Error ->
            Error
    end.

%% @doc Sync an invoice received via Stripe webhook.
sync_invoice(Inv, Context) ->
    Inv1 = stripe_invoice(Inv, Context),
    m_paysub:sync_invoice(stripe, Inv1, Context).

%% @doc Delete an invoice received via Stripe webhook.
-spec delete_invoice(Inv, Context) -> ok | {error, enoent} when
    Inv :: map(),
    Context :: z:context().
delete_invoice(#{ <<"id">> := Id }, Context) ->
    m_paysub:delete_invoice(stripe, Id, Context).


stripe_invoice(#{
        <<"id">> := InvId,
        <<"object">> := <<"invoice">>,
        <<"currency">> := Currency,
        <<"amount_due">> := AmountDue,
        <<"amount_paid">> := AmountPaid,
        <<"amount_remaining">> := AmountRemaing,
        <<"total">> := Total,
        <<"collection_method">> := CollectionMethod,
        <<"default_payment_method">> := PaymentMethod,
        <<"attempted">> := IsPaymentAttempted,
        <<"status">> := PaymentStatus,
        <<"created">> := Created,
        <<"period_end">> := PeriodEnd,
        <<"period_start">> := PeriodStart,
        <<"customer">> := CustId,
        <<"customer_email">> := CustomerEmail,
        <<"customer_name">> := CustomerName,
        <<"lines">> := #{
            <<"url">> := LineUrl
        } = Lines
    } = Inv, Context) ->
    Path = binary:split(LineUrl, <<"/">>, [ global ]),
    {ok, AllLines} = fetch_all({ok, Lines}, Path, #{}, [], Context),
    InvItems = lists:map(
        fun(#{
            <<"description">> := ItemDescription,
            <<"currency">> := ItemCurrency,
            <<"amount">> := ItemAmount,
            <<"proration">> := IsProration,
            <<"price">> := #{
                <<"id">> := ItemPriceId
            }
        }) ->
            #{
                description => ItemDescription,
                currency => z_string:to_upper(ItemCurrency),
                amount => ItemAmount,
                psp_price_id => ItemPriceId,
                is_prorated => IsProration
            }
        end,
        AllLines),
    Invoice = #{
        psp_invoice_id => InvId,
        psp_customer_id => CustId,
        currency => z_string:to_upper(Currency),
        total => Total,
        amount_due => AmountDue,
        amount_paid => AmountPaid,
        amount_remaining => AmountRemaing,
        collection_method => CollectionMethod,
        is_payment_attempted => IsPaymentAttempted,
        payment_status => PaymentStatus,
        payment_method => PaymentMethod,
        period_start => timestamp_to_datetime(PeriodStart),
        period_end => timestamp_to_datetime(PeriodEnd),
        created => timestamp_to_datetime(Created),
        name => CustomerName,
        email => CustomerEmail,
        items => InvItems
    },
    maybe_add_address(Inv, Invoice).

maybe_add_address(#{
        <<"customer_address">> := #{
            <<"country">> := CustomerCountry,
            <<"line1">> := CustomerLine1,
            <<"line2">> := CustomerLine2,
            <<"city">> := CustomerCity,
            <<"state">> := CustomerState,
            <<"postal_code">> := CustomerPostcode
        }
    }, Invoice) ->
    Invoice#{
        address_country => z_string:to_lower(CustomerCountry),
        address_street_1 => CustomerLine1,
        address_street_2 => CustomerLine2,
        address_city => CustomerCity,
        address_postcode => CustomerPostcode,
        address_state => CustomerState
    };
maybe_add_address(_, Invoice) ->
    Invoice.


timestamp_to_datetime(undefined) ->
    undefined;
timestamp_to_datetime(DT) ->
    z_datetime:timestamp_to_datetime(DT).

maybe_user_id(#{ <<"metadata">> := #{ <<"rsc_id">> := UserId }}) ->
    z_convert:to_integer(UserId);
maybe_user_id(#{ <<"metadata">> := #{ <<"user_id">> := UserId }}) ->
    z_convert:to_integer(UserId);
maybe_user_id(_) -> undefined.

pref_lang(#{ <<"preferred_locales">> := [ Locale | _ ]}, _Context) ->
    hd(binary:split(Locale, <<"-">>));
pref_lang(_, Context) ->
    z_context:language(Context).

adr_country(#{ <<"address">> := #{ <<"country">> := Country } }) when is_binary(Country) ->
    z_string:to_lower(Country);
adr_country(_) ->
    undefined.

fetch_all({ok, #{
        <<"data">> := List,
        <<"object">> := <<"list">>,
        <<"has_more">> := false
    }}, _Path, _Payload, Acc, _Context) ->
    {ok, Acc ++ List};
fetch_all({ok, #{
        <<"data">> := List,
        <<"object">> := <<"list">>,
        <<"has_more">> := true
    }}, Path, Payload, Acc, Context) ->
    P1 = Payload#{
        starting_after => last_id(List)
    },
    fetch_all(
        paysub_stripe_api:fetch(get, Path, P1, Context),
        Path,
        P1,
        Acc ++ List,
        Context);
fetch_all({error, _} = Error, _Path, _Payload, _Acc, _Context) ->
    Error.

last_id(L) ->
    [ #{ <<"id">> := Id } | _ ] = lists:reverse(L),
    Id.
