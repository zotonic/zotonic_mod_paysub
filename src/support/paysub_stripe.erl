%% @copyright 2022-2024 Marc Worrell
%% @doc Stripe support for payments and subscriptions.
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


% https://stripe.com/docs/billing/subscriptions/build-subscriptions
% https://docs.stripe.com/api/checkout/sessions/create
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
    checkout_session_completed/2,

    is_configured/1,

    is_customer/2,
    is_customer_portal/2,
    customer_portal_session_url/2,
    customer_portal_session_url/3,

    update_billing_address/2,
    update_customer_task/2,

    sync_product/2,
    sync_products/1,
    sync_prices/1,

    sync_customers/1,
    sync_customer/2,
    delete_customer/2,

    ensure_stripe_customer/2,

    sync_subscriptions/1,
    sync_subscription/2,
    delete_subscription/2,

    update_subscription_quantity/4,

    sync_invoices/1,
    sync_invoice/2,
    delete_invoice/2,

    sync_payments/1,
    sync_payment/2,
    delete_payment/2,

    schedule_sync/2,
    async_task/2,
    sync_task/3
    ]).


-include_lib("zotonic_core/include/zotonic.hrl").

%% @doc UI - checkout redirect
event(#postback{ message = {checkout, Args} }, Context) ->
    case checkout_session_create(Args, Context) of
        {ok, Url} ->
            z_render:wire({redirect, [ {location, Url} ]}, Context);
        {error, _} ->
            z_render:growl(?__("Sorry, something went wrong, please try again later.", Context), Context)
    end;
event(#postback{ message = {customer_portal, Args} }, Context) ->
    UserId = case proplists:get_value(id, Args) of
        undefined -> z_acl:user(Context);
        RscId -> RscId
    end,
    ReturnUrl = case proplists:get_value(return_url, Args) of
        RUrl when is_binary(RUrl) ->
            RUrl;
        Dispatch when is_atom(Dispatch) ->
            z_dispatcher:url_for(Dispatch, Context);
        undefined ->
            undefined
    end,
    case customer_portal_session_url(UserId, ReturnUrl, Context) of
        {ok, Url} ->
            z_render:wire({redirect, [ {location, Url} ]}, Context);
        {error, _} ->
            z_render:growl(?__("Sorry, something went wrong, please try again later.", Context), Context)
    end.

%% @doc Check if this user id has a known stripe customer id, if so then a
%% portal session is possible.
-spec is_customer(UserId, Context) -> boolean() when
    UserId :: m_rsc:resource(),
    Context :: z:context().
is_customer(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            false;
        RscId ->
            case z_acl:rsc_editable(RscId, Context)
                orelse is_allowed_portal_session(RscId, Context)
            of
                true ->
                    case m_paysub:get_customer(stripe, RscId, Context) of
                        {ok, #{
                            <<"psp_customer_id">> := _
                        }} ->
                            true;
                        {error, _} ->
                            false
                    end;
                false ->
                    false
            end
    end.

%% @doc Check if this user id has a known stripe customer id, if so then a
%% portal session is possible.
-spec is_customer_portal(UserId, Context) -> boolean() when
    UserId :: m_rsc:resource(),
    Context :: z:context().
is_customer_portal(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            false;
        RscId ->
            case is_allowed_portal_session(RscId, Context) of
                true ->
                    case m_paysub:get_customer(stripe, RscId, Context) of
                        {ok, #{
                            <<"psp_customer_id">> := _
                        }} ->
                            true;
                        {error, _} ->
                            false
                    end;
                false ->
                    false
            end
    end.

%% @doc Create a portal session for the user and return the URL to the portal.
-spec customer_portal_session_url(UserId, Context) -> {ok, Url} | {error, Reason} when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Url :: binary(),
    Reason :: enoent | term().
customer_portal_session_url(UserId, Context) ->
    customer_portal_session_url(UserId, undefined, Context).

%% @doc Create a portal session for the user and return the URL to the portal. If the
%% ReturnUrl is undefined then the user's page url is used instead.
-spec customer_portal_session_url(UserId, ReturnUrl, Context) -> {ok, Url} | {error, Reason} when
    UserId :: m_rsc:resource(),
    ReturnUrl :: binary() | undefined,
    Context :: z:context(),
    Url :: binary(),
    Reason :: enoent | eacces | term().
customer_portal_session_url(UserId, ReturnUrl, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            {error, enoent};
        UserRscId ->
            case is_allowed_portal_session(UserRscId, Context) of
                true ->
                    case m_paysub:get_customer(stripe, UserRscId, Context) of
                        {ok, #{
                            <<"psp_customer_id">> := CustomerId
                        }} ->
                            ReturnUrl1 = case ReturnUrl of
                                undefined -> m_rsc:p(UserRscId, page_url_abs, Context);
                                RUrl -> RUrl
                            end,
                            ReturnUrl2 = z_context:abs_url(ReturnUrl1, Context),
                            Locale = case m_rsc:p(UserRscId, pref_language, Context) of
                                undefined -> z_context:language(Context);
                                Lang -> Lang
                            end,
                            Payload = #{
                                customer => CustomerId,
                                return_url => ReturnUrl2,
                                locale => Locale
                            },
                            case paysub_stripe_api:fetch(post, ["billing_portal", "sessions"], Payload, Context) of
                                {ok, #{
                                    <<"url">> := Url
                                }} ->
                                    {ok, Url};
                                {error, Reason} = Error ->
                                    ?LOG_WARNING(#{
                                        in => zotonic_mod_paysub,
                                        text => <<"Error fetching Stripe billing portal URL">>,
                                        result => error,
                                        reason => Reason,
                                        psp => stripe,
                                        psp_customer_id => CustomerId,
                                        locale => Locale
                                    }),
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Only the subscriber (user) or the maincontact of the subscriber are allowed to
%% use the Stripe customer portal.
is_allowed_portal_session(Id, Context) ->
    Id =:= z_acl:user(Context) orelse m_paysub:is_user_maincontact(Id, Context).


%% @doc Start a Stripe checkout for the given payment/subscription request.
%% See documentation at https://docs.stripe.com/api/checkout/sessions/create
-spec checkout_session_create(Args, Context) -> {ok, Url} | {error, Reason} when
    Args :: proplists:proplist(),
    Context :: z:context(),
    Url :: binary(),
    Reason :: term().
checkout_session_create(Args, Context) ->
    UserId = z_acl:user(Context),
    case checkout_line_items(Args, Context) of
        {ok, {IsRecurring, IsUseMainContact, LineItems}} ->
            % For normal subscriptions the subscriber is the user.
            % For organisations with a 'has-maincontact' relation to the user
            % the subscriber is the (as yet undefined) organisation.
            SubscriberId = case IsUseMainContact of
                true ->
                    undefined;
                false ->
                    case proplists:lookup(subscriber_id, Args) of
                        {subscriber_id, SubId} -> SubId;
                        none -> UserId
                    end
            end,
            Mode = case z_convert:to_binary(proplists:get_value(mode, Args)) of
                <<>> when IsRecurring -> subscription;
                <<>> when not IsRecurring -> payment;
                <<"subscription">> -> subscription;
                <<"payment">> -> payment
            end,
            CheckoutProps = #{
                survey_id => proplists:get_value(survey_id, Args),
                survey_answer_id => proplists:get_value(survey_answer_id, Args),
                requestor_id => UserId,
                is_use_maincontact => IsUseMainContact,
                args => Args
            },
            {ok, CheckoutNr} = m_paysub:checkout_create(stripe, SubscriberId, Mode, CheckoutProps, Context),
            DoneUrl0 = z_dispatcher:url_for(paysub_psp_done, [ {checkout_nr, CheckoutNr} ], Context),
            DoneUrl = z_context:abs_url(DoneUrl0, Context),
            CancelUrl0 = case proplists:get_value(cancel_url, Args) of
                undefined -> z_dispatcher:url_for(paysub_psp_cancel, [ {checkout_nr, CheckoutNr} ], Context);
                <<>> -> z_dispatcher:url_for(paysub_psp_cancel, [ {checkout_nr, CheckoutNr} ], Context);
                CUrl -> CUrl
            end,
            CancelUrl = z_context:abs_url(CancelUrl0, Context),
            AdminUrl = case SubscriberId of
                undefined -> undefined;
                _ -> z_context:abs_url(z_dispatcher:url_for(admin_edit_rsc, [ {id, SubscriberId} ], Context), Context)
            end,
            CheckoutMetadata = proplists:get_value(metadata, Args, #{}),
            CheckoutMetadata1 = CheckoutMetadata#{
                requestor_id => UserId,
                rsc_id => SubscriberId,
                page_url => m_rsc:p_no_acl(SubscriberId, page_url_abs, Context),
                admin_url => AdminUrl,
                survey_answer_id => proplists:get_value(survey_answer_id, Args)
            },
            Payload = #{
                cancel_url => CancelUrl,
                success_url => DoneUrl,
                mode => Mode,
                client_reference_id => CheckoutNr,
                allow_promotion_codes => true,
                metadata => CheckoutMetadata1,
                line_items => LineItems,
                locale => z_context:language(Context)
            },
            IsTaxIdCollection = z_convert:to_bool(proplists:get_value(is_tax_id_collection, Args)),
            Payload1 = case IsTaxIdCollection of
                true ->
                    Payload#{
                        automatic_tax => #{
                            enabled => true
                        },
                        tax_id_collection => #{
                            enabled => true
                        }
                    };
                false ->
                    Payload
            end,
            Payload2 = case z_convert:to_binary(proplists:get_value(email, Args)) of
                <<>> ->
                    Payload1;
                Email ->
                    Payload1#{
                        customer_email => Email
                    }
            end,
            IsBillingAddressCollection = z_convert:to_bool(proplists:get_value(is_billing_address_collection, Args, true)),
            BaseSub = case z_convert:to_integer(proplists:get_value(trial_days, Args, 0)) of
                N when is_integer(N), N >= 1 -> #{ trial_period_days => N };
                _ -> #{}
            end,
            Payload3 = case m_paysub:get_customer(stripe, SubscriberId, Context) of
                {ok, #{
                    <<"psp_customer_id">> := CustId
                }} when is_binary(CustId) ->
                    % Only customer or customer_email can be used.
                    PE1 = maps:remove(customer_email, Payload2#{
                        customer => CustId,
                        customer_update => #{
                            address => auto,
                            name => auto
                        }
                    }),
                    case Mode of
                        subscription ->
                            PE1#{
                                billing_address_collection =>
                                    case IsBillingAddressCollection of
                                        true -> required;
                                        false -> auto
                                    end
                            };
                        payment ->
                            PE1#{
                                billing_address_collection => auto
                            }
                    end;
                {error, enoent} when is_integer(SubscriberId) ->
                    PE1 = case Payload2 of
                        #{
                            customer_email := _
                        } ->
                            Payload2;
                        _ ->
                            case m_rsc:p_no_acl(SubscriberId, email_raw, Context) of
                                undefined ->
                                    Payload2;
                                UserEmail ->
                                    Payload2#{
                                        customer_email => UserEmail
                                    }
                            end
                    end,
                    PE2 = PE1#{
                        consent_collection => #{
                            terms_of_service => required
                        }
                    },
                    case Mode of
                        subscription ->
                            PE2#{
                                billing_address_collection =>
                                    case IsBillingAddressCollection of
                                        true -> required;
                                        false -> auto
                                    end,
                                subscription_data => BaseSub#{
                                    metadata => #{
                                        rsc_id => SubscriberId,
                                        page_url => m_rsc:p_no_acl(SubscriberId, page_url_abs, Context),
                                        admin_url => AdminUrl
                                    }
                                }
                            };
                        payment ->
                            PE2#{
                                customer_creation => always,
                                billing_address_collection => auto,
                                payment_intent_data => #{
                                    description => proplists:get_value(description, Args, undefined),
                                    metadata => CheckoutMetadata1
                                },
                                invoice_creation => #{
                                    enabled => true
                                }
                            }
                    end;
                {error, enoent} when SubscriberId =:= undefined, Mode =:= payment ->
                    Payload2#{
                        customer_creation => if_required,
                        billing_address_collection => auto,
                        consent_collection => #{
                            terms_of_service => required
                        },
                        invoice_creation => #{
                            enabled => true
                        },
                        payment_intent_data => #{
                            description => proplists:get_value(description, Args, undefined),
                            metadata => CheckoutMetadata1
                        }
                    };
                {error, enoent} when SubscriberId =:= undefined, Mode =:= subscription ->
                    Payload2#{
                        billing_address_collection =>
                            case IsBillingAddressCollection of
                                true -> required;
                                false -> auto
                            end,
                        consent_collection => #{
                            terms_of_service => required
                        },
                        subscription_data => BaseSub
                    }
            end,
            Payload4 = case z_convert:to_binary(proplists:get_value(currency, Args)) of
                <<>> -> Payload3;
                RequestedCurrency ->
                    Payload3#{
                        currency => RequestedCurrency
                    }
            end,
            case paysub_stripe_api:fetch(post, [ "checkout", "sessions" ], Payload4, Context) of
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
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe: Unable to create checkout session.">>,
                        result => error,
                        reason => Reason,
                        psp => stripe,
                        args => Args,
                        line_items => LineItems,
                        mode => Mode,
                        checkout_session => CheckoutNr,
                        user_id => UserId,
                        subscriber_id => SubscriberId
                    }),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: price for checkout link could not be fetched.">>,
                result => error,
                reason => Reason,
                psp => stripe,
                args => Args
            }),
            Error
    end.

checkout_line_items(Args, Context) ->
    QuantityArg = z_convert:to_integer(proplists:get_value(quantity, Args)),
    Quantity = if QuantityArg =:= undefined -> 1; true -> QuantityArg end,
    case proplists:get_value(price, Args) of
        undefined ->
            case proplists:get_value(line_items, Args, []) of
                [] ->
                    {error, noprice};
                LineItems when is_list(LineItems) ->
                    lists:foldr(
                        fun
                            (_Item, {error, _} = Error) ->
                                Error;
                            (#{ price := PriceName } = Item,
                             {ok, {IsRecurringAcc, IsUseMainContactAcc, LineAcc}}) ->
                                case m_paysub:get_price(stripe, PriceName, Context) of
                                    {ok, #{
                                        <<"psp_price_id">> := PriceId,
                                        <<"is_recurring">> := IsRecurringPrice,
                                        <<"is_use_maincontact">> := IsUseMainContact,
                                        <<"billing_scheme">> := BillingScheme
                                    }} ->
                                        NewQuantity = case BillingScheme of
                                            <<"tiered">> ->
                                                case QuantityArg of
                                                    undefined -> maps:get(quantity, Item, Quantity);
                                                    _ -> Quantity
                                                end;
                                            _ ->
                                                maps:get(quantity, Item, Quantity)
                                        end,
                                        LineAcc1 = [
                                           #{
                                                price => PriceId,
                                                quantity => NewQuantity
                                            }
                                            | LineAcc
                                        ],
                                        IsRecurringAcc1 = IsRecurringAcc orelse IsRecurringPrice,
                                        IsUseMainContactAcc1 = IsUseMainContactAcc orelse IsUseMainContact,
                                        {ok, {IsRecurringAcc1, IsUseMainContactAcc1, LineAcc1}};
                                    {error, _} = Error ->
                                        Error
                                end
                        end,
                        {ok, {false, false, []}},
                        LineItems)
            end;
        PriceName ->
            case m_paysub:get_price(stripe, PriceName, Context) of
                {ok, #{
                    <<"psp_price_id">> := PriceId,
                    <<"is_recurring">> := IsRecurring,
                    <<"is_use_maincontact">> := IsUseMainContact
                }} ->
                    LineItems = [
                        #{
                            price => PriceId,
                            quantity => Quantity
                        }
                    ],
                    {ok, {IsRecurring, IsUseMainContact, LineItems}};
                {error, _} = Error ->
                    Error
            end
    end.


%% @doc Called by the controller_subscription_stripe_redirect to handle incoming
%% webhook events.
-spec checkout_session_sync(Session, Context) -> ok | {error, Reason} when
    Session :: map() | binary(),
    Context :: z:context(),
    Reason :: term().
checkout_session_sync(SessionId, Context) when is_binary(SessionId) ->
    case paysub_stripe_api:fetch(get, [ <<"checkout">>, <<"sessions">>, SessionId ], undefined, Context) of
        {ok, Session} ->
            checkout_session_sync(Session, Context);
        {error, _} = Error ->
            Error
    end;
checkout_session_sync(Session, Context) ->
    #{
        <<"id">> := _CheckoutId,
        <<"customer">> := CustomerId,
        <<"status">> := Status,
        <<"payment_intent">> := PaymentId,
        <<"payment_status">> := PaymentStatus,
        <<"client_reference_id">> := CheckoutNr,
        <<"currency">> := Currency,
        <<"amount_total">> := Amount
    } = Session,
    Update = #{
        psp_customer_id => CustomerId,
        psp_payment_id => PaymentId,
        status => Status,
        payment_status => PaymentStatus,
        currency => z_string:to_upper(Currency),
        amount => Amount
    },
    m_paysub:checkout_update(stripe, CheckoutNr, Update, Context),
    ok.


%% @doc Called on by controller_subscription_stripe_redirect on a successful checkout
%% session. We can now ensure the creation of the customer and provision the subscription.
-spec checkout_session_completed(Session, Context) -> ok | {error, Reason} when
    Session :: map(),
    Context :: z:context(),
    Reason :: term().
checkout_session_completed(#{ <<"mode">> := <<"subscription">> } = Session, Context) ->
    checkout_session_sync(Session, Context),
    #{
        <<"customer">> := PSPCustomerId,
        <<"client_reference_id">> := CheckoutNr,
        <<"subscription">> := PSPSubscriptionId,
        <<"payment_status">> := _PaymentStatus,
        <<"metadata">> := Metadata
        % "customer_details": {
        %   "address": {
        %     "city": "Amsterdam",
        %     "country": "NL",
        %     "line1": "Foobar 24",
        %     "line2": null,
        %     "postal_code": "1234 AS",
        %     "state": null
        %   },
        %   "email": "test@example.com",
        %   "name": "T Tester (admin)",
        %   "phone": null,
        %   "tax_exempt": "none",
        %   "tax_ids": [
        %   ]
        % },
        % "customer_email": "test@example.com",
        % "metadata": {
        %   "admin_url": "https://example.com/en/admin/edit/1",
        %   "page_url": "https://example.com/en/page/1",
        %   "rsc_id": "1"
        % },
    } = Session,
    % 1. Ensure we have received the customer id in our db
    Customer = case m_paysub:get_customer(stripe, PSPCustomerId, Context) of
        {ok, Cust} ->
            Cust;
        {error, enoent} ->
            ok = sync_customer(PSPCustomerId, Context),
            {ok, Cust} = m_paysub:get_customer(stripe, PSPCustomerId, Context),
            Cust
    end,
    % 2. Ensure we have received the subscription id in our db
    case m_paysub:get_subscription(stripe, PSPSubscriptionId, Context) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            ok = sync_subscription(PSPSubscriptionId, Context)
    end,
    % 3. Maybe sync the rsc id on the customer
    case Customer of
        #{ <<"rsc_id">> := undefined } ->
            case Metadata of
                #{ <<"rsc_id">> := RscId } when is_integer(RscId) ->
                    ok = m_paysub:update_customer_rsc_id(stripe, PSPCustomerId, RscId, Context);
                _ ->
                    ok
            end;
        #{ <<"rsc_id">> := _ } ->
            ok
    end,
    % 4. Provision the subscription by notifying paysub_subscription with status 'new'
    m_paysub:provision_subscription(stripe, PSPSubscriptionId, CheckoutNr, Context);
checkout_session_completed(#{ <<"mode">> := <<"payment">> } = Session, Context) ->
    % 1. Save the new Session
    checkout_session_sync(Session, Context),
    % 2. If payment complete then send notification
    #{
        <<"client_reference_id">> := CheckoutNr
    } = Session,
    m_paysub:checkout_completed(stripe, CheckoutNr, Context).


%% @doc Check if a Stripe API key is configured. If so then it can be assumed that Stripe
%% can be used.
-spec is_configured(Context) -> boolean() when
    Context :: z:context().
is_configured(Context) ->
    not z_utils:is_empty(m_config:get_value(mod_paysub, stripe_api_key, <<>>, Context)).


%% @doc Sync the resource billing address to the customer at Stripe.
-spec update_billing_address(CustId, Context) -> ok | {error, Reason} when
    CustId :: binary() | m_rsc:resource_id(),
    Context :: z:context(),
    Reason :: retry | term().
update_billing_address(CustId, Context) ->
    case m_paysub:get_customer(stripe, CustId, Context) of
        {ok, #{
            <<"rsc_id">> := Id,
            <<"psp_customer_id">> := PSPCustomerId
        }} when is_integer(Id) ->
            case m_paysub:billing_address(Id, Context) of
                {ok, #{
                    <<"country">> := Country,
                    <<"street_1">> := Street1,
                    <<"street_2">> := Street2,
                    <<"city">> := City,
                    <<"state">> := State,
                    <<"postcode">> := Postcode,
                    <<"email">> := Email
                }} ->
                    ContextSudo = z_acl:sudo(Context),
                    AdminUrl = z_dispatcher:url_for(admin_edit_rsc, [ {id, Id} ], Context),
                    Address = #{
                        <<"country">> => z_string:to_upper(Country),
                        <<"city">> => City,
                        <<"line1">> => Street1,
                        <<"line2">> => Street2,
                        <<"state">> => State,
                        <<"postal_code">> => Postcode
                    },
                    Customer = #{
                        <<"address">> => Address,
                        <<"email">> => Email,
                        <<"phone">> => truncate(m_paysub:billing_phone(Id, Context), 19),
                        <<"name">> => m_paysub:billing_name(Id, Context),
                        <<"preferred_locales">> => [ bin(pref_language(Id, ContextSudo)) ],
                        <<"metadata">> => #{
                            <<"rsc_id">> => Id,
                            <<"page_url">> => m_rsc:p_no_acl(Id, page_url_abs, ContextSudo),
                            <<"admin_url">> => z_context:abs_url(AdminUrl, Context)
                        }
                    },
                    case paysub_stripe_api:fetch(post, [ <<"customers">>, PSPCustomerId ], Customer, Context) of
                        {ok, #{ <<"id">> := PSPCustId } = CustObject} ->
                            sync_customer(CustObject, Context),
                            ?LOG_INFO(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe updated billing information for customer from resource">>,
                                result => ok,
                                rsc_id => Id,
                                psp_customer_id => PSPCustId,
                                psp => stripe
                            }),
                            ok;
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Stripe error updating billing information for customer from resource">>,
                                rsc_id => Id,
                                result => error,
                                reason => Reason,
                                psp => stripe
                            }),
                            {error, reason_retry(Reason)}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

reason_retry(timeout) -> retry;
reason_retry(nxdomain) -> retry;
reason_retry(Reason) -> Reason.


%% @doc Update the rsc_id, metadata and billing address of customer in Stripe. This is called from
%% a async task, and can return a {delay, ...} tuple if Stripe can't be reached.
-spec update_customer_task(PSPCustomerId, Context) -> ok | {delay, pos_integer()} when
    PSPCustomerId :: binary(),
    Context :: z:context().
update_customer_task(PSPCustomerId, Context) ->
    case m_paysub:get_customer(stripe, PSPCustomerId, Context) of
        {ok, #{
            <<"rsc_id">> := RscId
        }} ->
            Cust = case RscId of
                undefined ->
                    #{
                        <<"metadata">> => #{
                            <<"rsc_id">> => undefined,
                            <<"page_url">> => undefined,
                            <<"admin_url">> => undefined
                        }
                    };
                _ ->
                    ContextSudo = z_acl:sudo(Context),
                    AdminUrl = z_dispatcher:url_for(admin_edit_rsc, [ {id, RscId} ], Context),
                    {Name, _} = z_template:render_to_iolist("_name.tpl", [ {id, RscId} ], ContextSudo),
                    #{
                        <<"name">> => z_string:trim(iolist_to_binary(Name)),
                        <<"address">> => billing_address(RscId, ContextSudo),
                        <<"email">> => billing_email(RscId, ContextSudo),
                        <<"phone">> => truncate(m_rsc:p_no_acl(RscId, <<"phone">>, ContextSudo), 19),
                        <<"preferred_locales">> => [ bin(pref_language(RscId, ContextSudo)) ],
                        <<"metadata">> => #{
                            <<"rsc_id">> => RscId,
                            <<"page_url">> => m_rsc:p_no_acl(RscId, page_url_abs, ContextSudo),
                            <<"admin_url">> => z_context:abs_url(AdminUrl, Context)
                        }
                    }
            end,
            Path = [ <<"customers">>, PSPCustomerId ],
            case paysub_stripe_api:fetch(post, Path, Cust, Context) of
                {ok, _} ->
                    ?LOG_INFO(#{
                        in => zotonic_mod_paysub,
                        text => <<"Updated customer details at Stripe">>,
                        result => ok,
                        psp => stripe,
                        psp_customer_id => PSPCustomerId,
                        rsc_id => RscId
                    }),
                    ok;
                {error, Reason} when Reason =:= enoent ->
                    ?LOG_WARNING(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe error updating customer details">>,
                        result => error,
                        reason => Reason,
                        psp => stripe,
                        psp_customer_id => PSPCustomerId,
                        rsc_id => RscId
                    }),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_paysub,
                        text => <<"Stripe error updating customer details">>,
                        result => error,
                        reason => Reason,
                        psp => stripe,
                        psp_customer_id => PSPCustomerId,
                        rsc_id => RscId
                    }),
                    {delay, 600}
            end;
        {error, enoent} ->
            ok
    end.


%% @doc Sync all products in Stripe with products here.
-spec sync_product(PspProdId, Context) -> ok | {error, term()} when
    PspProdId :: binary(),
    Context :: z:context().
sync_product(PspProdId, Context) ->
    Path = [ <<"products">>, PspProdId ],
    case paysub_stripe_api:fetch(get, Path, #{}, Context) of
        {ok, Prod} ->
            Prod1 = stripe_prod(Prod, Context),
            ok = m_paysub:sync_products(stripe, [ Prod1 ], Context),
            ?LOG_INFO(#{
                in => zotonic_mod_paysub,
                text => <<"Stripe: sync single product">>,
                result => ok,
                psp => stripe,
                product_id => PspProdId
            }),
            ok;
        {error, _} = Error ->
            Error
    end.

%% @doc Sync all products in Stripe with products here.
-spec sync_products(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_products(Context) ->
    Payload = #{
        limit => 50,
        type => <<"service">>
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
        <<"lookup_key">> := Key,
        <<"product">> := ProductId,
        <<"currency">> := Currency,
        <<"unit_amount">> := UnitAmount,
        <<"billing_scheme">> := BillingScheme,
        <<"tiers_mode">> := TiersMode,
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
        key => z_convert:to_binary(Key),
        currency => z_string:to_upper(Currency),
        amount => UnitAmount,
        billing_scheme => BillingScheme,
        tiers_mode => TiersMode
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


-spec sync_task(What, StripeId, Context) -> ok | Retry when
    What :: customer | subscription | payment | invoice,
    StripeId :: binary(),
    Context :: z:context(),
    Retry :: {delay, pos_integer()}.
sync_task(What, StripeId, Context) ->
    case sync_task_do(What, StripeId, Context) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, _} ->
            {delay, 120}
    end.

sync_task_do(customer, Id, Context) ->
    sync_customer(Id, Context);
sync_task_do(subscription, Id, Context) ->
    sync_subscription(Id, Context);
sync_task_do(payment, Id, Context) ->
    sync_payment(Id, Context);
sync_task_do(invoice, Id, Context) ->
    sync_invoice(Id, Context).

%% @doc Sync a single customer after a webhook event.
-spec sync_customer(Cust, Context) -> ok | {error, Reason} when
    Cust :: map() | binary(),
    Context :: z:context() | atom(),
    Reason :: term().
sync_customer(CustId, Site) when is_atom(Site) ->
    Context = z_context:new(Site),
    sync_customer(CustId, Context);
sync_customer(CustId, Context) when is_binary(CustId) ->
    case paysub_stripe_api:fetch(get, [ <<"customers">>, CustId ], undefined, Context) of
        {ok, Cust} ->
            case sync_customer(Cust, Context) of
                ok ->
                    UniqueKey = <<"paysub-stripe-", CustId/binary>>,
                    z_pivot_rsc:delete_task(paysub_stripe, sync_task, UniqueKey, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
sync_customer(Cust, Context) ->
    SCust = stripe_customer(Cust, Context),
    m_paysub:sync_customer(stripe, SCust, Context).

%% @doc Sync all customers in Stripe with customers here.
-spec sync_customers(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: term().
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
        phone => maps:get(<<"phone">>, Cust, undefined)
    },
    P1 = maybe_add_address(Cust, P),
    case maybe_user_id(Cust, Context) of
        undefined ->
            P1;
        UserId ->
            UserId1 = case m_rsc:exists(UserId, Context) of
                true -> UserId;
                false -> undefined
            end,
            P1#{
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
                    AdminUrl = z_dispatcher:url_for(admin_edit_rsc, [ {id, RscId} ], Context),
                    Cust = #{
                        <<"address">> => billing_address(RscId, ContextSudo),
                        <<"email">> => z_html:unescape(bin(billing_email(RscId, ContextSudo))),
                        <<"phone">> => truncate(m_paysub:billing_phone(RscId, ContextSudo), 19),
                        <<"name">> => m_paysub:billing_name(RscId, ContextSudo),
                        <<"preferred_locales">> => [ bin(pref_language(RscId, ContextSudo)) ],
                        <<"metadata">> => #{
                            <<"rsc_id">> => RscId,
                            <<"page_url">> => m_rsc:p_no_acl(RscId, <<"page_url_abs">>, ContextSudo),
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

truncate(undefined, _Len) ->
    undefined;
truncate(B, Len) ->
    z_string:truncatechars(B, Len).

billing_address(Id, Context) ->
    case m_paysub:billing_address(Id, Context) of
        {ok, Addr} ->
            #{
                <<"line1">> => maps:get(<<"street_1">>, Addr),
                <<"line2">> => maps:get(<<"street_2">>, Addr),
                <<"postal_code">> => maps:get(<<"postcode">>, Addr),
                <<"city">> => maps:get(<<"city">>, Addr),
                <<"state">> => maps:get(<<"state">>, Addr),
                <<"country">> => z_string:to_upper(maps:get(<<"country">>, Addr))
            };
        {error, _} ->
            #{}
    end.

billing_email(Id, Context) ->
    BillingEmail = m_rsc:p_no_acl(Id, <<"billing_email">>, Context),
    case z_utils:is_empty(BillingEmail) of
        true -> m_rsc:p_no_acl(Id, <<"email">>, Context);
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
    Sub :: map() | binary(),
    Context :: z:context() | atom().
sync_subscription(SubId, Site) when is_atom(Site) ->
    Context = z_context:new(Site),
    sync_subscription(SubId, Context);
sync_subscription(SubId, Context) when is_binary(SubId) ->
    case paysub_stripe_api:fetch(get, [ <<"subscriptions">>, SubId ], undefined, Context) of
        {ok, Sub} ->
            case sync_subscription(Sub, Context) of
                ok ->
                    UniqueKey = <<"paysub-stripe-", SubId/binary>>,
                    z_pivot_rsc:delete_task(paysub_stripe, sync_task, UniqueKey, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
sync_subscription(Sub, Context) when is_map(Sub) ->
    SSub = stripe_subscription(Sub, Context),
    PriceItems = stripe_subscription_prices(Sub),
    ok = m_paysub:sync_subscription(stripe, SSub, PriceItems, Context),
    OptRscId = maps:get(rsc_id, SSub, undefined),
    CustomerId = maps:get(psp_customer_id, SSub),
    maybe_set_customer_rsc_id(OptRscId, CustomerId, Context).

stripe_subscription(#{
        <<"id">> := Id,
        <<"customer">> := CustomerId,
        <<"status">> := Status,
        <<"canceled_at">> := CanceledAt,
        <<"cancel_at">> := CancelAt,
        <<"start_date">> := StartDate,
        <<"ended_at">> := EndDate,
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
        started_at => timestamp_to_datetime(StartDate),
        ended_at => timestamp_to_datetime(EndDate),
        period_start => timestamp_to_datetime(CurrPeriodStart),
        period_end => timestamp_to_datetime(CurrPeriodEnd),
        trial_start => timestamp_to_datetime(TrialStart),
        trial_end => timestamp_to_datetime(TrialEnd),
        canceled_at => timestamp_to_datetime(CanceledAt),
        cancel_at => timestamp_to_datetime(CancelAt)
    },
    case m_rsc:rid(maybe_user_id(Sub, Context), Context) of
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
            <<"id">> := Id,
            <<"object">> := <<"subscription_item">>,
            <<"price">> := #{
                <<"id">> := PriceId
            },
            <<"quantity">> := Quantity
        }) ->
            #{
                psp_price_id => PriceId,
                psp_item_id => Id,
                quantity => Quantity
            }
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


%% @doc Update the quantity of a subscription. The firts tiered price in the subscripton items
%% will have its quantities updated.
-spec update_subscription_quantity(SubId, ItemId, Quantity, Context) -> ok | {error, enoent} when
    SubId :: binary(),
    ItemId :: binary(),
    Quantity :: integer(),
    Context :: z:context().
update_subscription_quantity(SubId, ItemId, Quantity, Context) ->
    Payload = #{
        items => [
            #{
                id => ItemId,
                quantity => Quantity
            }
        ]
    },
    Path = [ <<"subscriptions">>, SubId ],
    case paysub_stripe_api:fetch(post, Path, Payload, Context) of
        {ok, Sub} ->
            sync_subscription(Sub, Context),
            ok;
        {error, _} = Error ->
            Error
    end.


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
sync_invoice(InvId, Site) when is_atom(Site) ->
    Context = z_context:new(Site),
    sync_invoice(InvId, Context);
sync_invoice(InvId, Context) when is_binary(InvId) ->
    case paysub_stripe_api:fetch(get, [ <<"invoices">>, InvId ], undefined, Context) of
        {ok, Inv} ->
            case sync_invoice(Inv, Context) of
                ok ->
                    UniqueKey = <<"paysub-stripe-", InvId/binary>>,
                    z_pivot_rsc:delete_task(paysub_stripe, sync_task, UniqueKey, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
sync_invoice(Inv, Context) when is_map(Inv) ->
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
    }, Rec) ->
    Rec#{
        address_country => z_string:to_lower(CustomerCountry),
        address_street_1 => CustomerLine1,
        address_street_2 => CustomerLine2,
        address_city => CustomerCity,
        address_postcode => CustomerPostcode,
        address_state => CustomerState
    };
maybe_add_address(#{
        <<"address">> := #{
            <<"country">> := CustomerCountry,
            <<"line1">> := CustomerLine1,
            <<"line2">> := CustomerLine2,
            <<"city">> := CustomerCity,
            <<"state">> := CustomerState,
            <<"postal_code">> := CustomerPostcode
        }
    }, Rec) ->
    Rec#{
        address_country => z_string:to_lower(CustomerCountry),
        address_street_1 => CustomerLine1,
        address_street_2 => CustomerLine2,
        address_city => CustomerCity,
        address_postcode => CustomerPostcode,
        address_state => CustomerState
    };
maybe_add_address(_, Rec) ->
    Rec.

timestamp_to_datetime(undefined) ->
    undefined;
timestamp_to_datetime(DT) ->
    z_datetime:timestamp_to_datetime(DT).

maybe_user_id(#{ <<"metadata">> := #{ <<"rsc_id">> := UserId }}, _Context) ->
    z_convert:to_integer(UserId);
maybe_user_id(#{ <<"metadata">> := #{ <<"user_id">> := UserId }}, _Context) ->
    z_convert:to_integer(UserId);
maybe_user_id(#{ <<"customer">> := CustId }, Context) when is_binary(CustId) ->
    case m_paysub:get_customer(stripe, CustId, Context) of
        {ok, #{ <<"rsc_id">> := UserId }} -> UserId;
        {error, _} -> undefined
    end;
maybe_user_id(_, _Context)  ->
    undefined.

pref_lang(#{ <<"preferred_locales">> := [ Locale | _ ]}, _Context) ->
    hd(binary:split(Locale, <<"-">>));
pref_lang(_, Context) ->
    z_context:language(Context).


%% @doc Sync all payment intents in Stripe with payments here.
-spec sync_payments(Context) -> ok | {error, term()} when
    Context :: z:context().
sync_payments(Context) ->
    Payload = #{
        limit => 50
    },
    Path = [ <<"payment_intents">> ],
    case fetch_all(
        paysub_stripe_api:fetch(get, Path, Payload, Context),
        Path,
        Payload,
        [],
        Context)
    of
        {ok, Payments} ->
            Payments1 = lists:map(
                fun(Inv) ->
                    stripe_payment(Inv, Context)
                end, Payments),
            m_paysub:sync_payments(stripe, Payments1, Context);
        {error, _} = Error ->
            Error
    end.


%% @doc Sync a payment intent received via Stripe webhook.
sync_payment(PaymentId, Site) when is_atom(Site) ->
    Context = z_context:new(Site),
    sync_payment(PaymentId, Context);
sync_payment(PaymentId, Context) when is_binary(PaymentId) ->
    case paysub_stripe_api:fetch(get, [ <<"payment_intents">>, PaymentId ], undefined, Context) of
        {ok, Payment} ->
            case sync_payment(Payment, Context) of
                ok ->
                    UniqueKey = <<"paysub-stripe-", PaymentId/binary>>,
                    z_pivot_rsc:delete_task(paysub_stripe, sync_task, UniqueKey, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
sync_payment(PaymentObject, Context) ->
    Payment1 = stripe_payment(PaymentObject, Context),
    m_paysub:sync_payment(stripe, Payment1, Context).

%% @doc Delete an invoice received via Stripe webhook.
-spec delete_payment(PaymentObject, Context) -> ok | {error, enoent} when
    PaymentObject :: map(),
    Context :: z:context().
delete_payment(#{ <<"id">> := Id }, Context) ->
    m_paysub:delete_payment(stripe, Id, Context).


stripe_payment(#{
        <<"id">> := Id,
        <<"status">> := Status,
        <<"description">> := Description,
        <<"customer">> := CustomerId,
        <<"currency">> := Currency,
        <<"amount">> := Amount,
        <<"amount_received">> := AmountReceived,
        <<"invoice">> := InvoiceId,
        <<"created">> := Created
    } = P, _Context) ->
    Payment = payment_name_details(P),
    Payment#{
        psp_payment_id => Id,
        psp_customer_id => CustomerId,
        psp_invoice_id => InvoiceId,
        status => Status,
        description => Description,
        currency => z_string:to_upper(Currency),
        amount => Amount,
        amount_received => AmountReceived,
        created => timestamp_to_datetime(Created)
    }.

payment_name_details(#{ <<"charges">> := #{ <<"data">> := Data }}) ->
    payment_charge_details(Data);
payment_name_details(_) ->
    #{}.

payment_charge_details([]) ->
    #{};
payment_charge_details([
    #{
        <<"billing_details">> := #{
            <<"email">> := Email,
            <<"name">> := Name,
            <<"phone">> := Phone
        }
    } | _ ]) when
        Name =/= undefined;
        Email =/= undefined;
        Phone =/= undefined ->
    #{
        name => Name,
        email => Email,
        phone => Phone
    };
payment_charge_details([_|Cs]) ->
    payment_charge_details(Cs).


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
