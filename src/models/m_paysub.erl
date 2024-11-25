%% @copyright 2022-2024 Marc Worrell
%% @doc Model for paid subscriptions
%% @end

%% Stripe Subscription status model
%% ================================
%%
%% Possible values are: incomplete, incomplete_expired, trialing, active, past_due, canceled, or unpaid.
%%
%% For collection_method=charge_automatically a subscription moves into incomplete if the initial payment
%% attempt fails. A subscription in this state can only have metadata and default_payment_method updated. Once
%% the first invoice is paid, the subscription moves into an active state. If the first invoice is not
%% paid within 23 hours, the subscription transitions to incomplete_expired. This is a terminal state,
%% the open invoice will be voided and no further invoices will be generated.
%%
%% A subscription that is currently in a trial period is trialing and moves to active when the trial period
%% is over.
%%
%% If subscription collection_method=charge_automatically it becomes past_due when payment to renew
%% it fails and canceled or unpaid (depending on your subscriptions settings) when Stripe has exhausted all
%% payment retry attempts.
%%
%% If subscription collection_method=send_invoice it becomes past_due when its invoice is not paid by the
%% due date, and canceled or unpaid if it is still not paid by an additional deadline after that. Note that
%% when a subscription has a status of unpaid, no subsequent invoices will be attempted (invoices will
%% be created, but then immediately automatically closed). After receiving updated payment information from
%% a customer, you may choose to reopen and pay their closed invoices.


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


-module(m_paysub).

-export([
    m_get/3,

    subscription_products/1,
    subscription_prices/1,

    access_states/1,
    active_states/0,
    pending_states/1,
    inactive_states/1,
    all_states/0,

    billing_address/2,
    billing_email/2,
    billing_name/2,
    billing_phone/2,

    sync_billing_to_rsc/3,
    sync_billing_to_psp/2,
    sync_billing_to_psp/3,
    task_sync_billing_to_psp/3,

    is_subscriber/2,
    is_allowed_paysub/1,
    is_user_maincontact/2,

    overview_ug/1,

    count_invoices/1,
    count_user_invoices/2,
    count_subscriptions/1,
    count_payments/1,
    count_user_payments/2,
    count_products/1,

    user_groups/2,
    users_groups/2,

    user_subscriptions/2,
    user_customers/2,
    user_invoices/2,
    user_payments/2,

    checkout_status/2,
    checkout_create/5,
    checkout_update/4,
    checkout_completed/3,
    is_survey_checkout/2,
    survey_checkout_status/3,

    get_customer/3,

    get_subscription/2,
    get_subscription/3,

    list_products/2,
    list_prices/2,
    list_customers/2,

    sync_products/3,
    sync_product/3,
    get_product/3,
    update_product/3,
    delete_product/3,

    sync_prices/3,
    sync_price/3,
    delete_price/3,
    get_price/3,

    sync_payments/3,
    sync_payment/3,
    delete_payment/3,

    maybe_provision_user_subscriptions_async/2,
    provision_user_subscriptions/2,
    provision_subscription/4,
    sync_subscription/4,
    update_subscription/3,
    delete_subscription/3,

    subscription_add_prices/2,

    sync_customer_rsc_id/2,

    update_customer_rsc_id/4,
    update_customer_rsc_id/5,
    update_customer_psp/3,
    update_customer_psp_task/3,

    sync_customer/3,
    sync_customers/3,
    delete_customer/3,

    sync_invoice/3,
    sync_invoices/3,
    delete_invoice/3,

    rsc_merge/3,
    move_subscriptions/4,
    has_moveable_maincontact_subs/2,

    products_add_prices/2,

    notify_subscription/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("epgsql/include/epgsql.hrl").
-include("../include/paysub.hrl").


-type customer() :: #{ binary() => term() }.
-type subscription() :: #{ binary() => term() }.
-type checkout_status() :: #{ binary() => term() }.

-export_type([
    customer/0,
    subscription/0,
    checkout_status/0
]).

m_get([ <<"is_allowed_paysub">> | Rest ], _Msg, Context) ->
    {ok, {is_allowed_paysub(Context), Rest}};
m_get([ <<"is_subscriber">> ], _Msg, Context) ->
    IsSubscriber = is_subscriber(z_acl:user(Context), Context),
    {ok, {IsSubscriber, []}};
m_get([ <<"is_subscriber">>, Id | Rest ], _Msg, Context) ->
    IsSubscriber = is_subscriber(Id, Context),
    {ok, {IsSubscriber, Rest}};
m_get([ <<"is_customer_portal">>, <<"stripe">> ], _Msg, Context) ->
    {ok, {paysub_stripe:is_customer_portal(z_acl:user(Context), Context), []}};
m_get([ <<"is_customer_portal">>, <<"stripe">>, Id | Rest ], _Msg, Context) ->
    {ok, {paysub_stripe:is_customer_portal(Id, Context), Rest}};
m_get([ <<"is_customer">>, <<"stripe">>, Id | Rest ], _Msg, Context) ->
    {ok, {paysub_stripe:is_customer(Id, Context), Rest}};
m_get([ <<"is_unpaid_access">> | Rest ], _Msg, Context) ->
    {ok, {m_config:get_boolean(mod_paysub, is_unpaid_access, Context), Rest}};
m_get([ <<"customer_portal_session_url">>, <<"stripe">> ], _Msg, Context) ->
    case paysub_stripe:customer_portal_session_url(z_acl:user(Context), Context) of
        {ok, Url} ->
            {ok, {Url, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"customer_portal_session_url">>, <<"stripe">>, Id | Rest ], _Msg, Context) ->
    case paysub_stripe:customer_portal_session_url(Id, Context) of
        {ok, Url} ->
            {ok, {Url, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"customer_url">>, <<"stripe">>, Id | Rest ], _Msg, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            {error, enoent};
        RId ->
            case z_acl:rsc_visible(RId, Context) andalso is_allowed_paysub(Context) of
                true ->
                    case m_paysub:get_customer(stripe, RId, Context) of
                        {ok, #{ <<"psp_customer_id">> := PspCustId }} when is_binary(PspCustId) ->
                            Url = iolist_to_binary([
                                    <<"https://dashboard.stripe.com/customers/">>,
                                    z_url:percent_encode(PspCustId)
                                ]),
                            {ok, {Url, Rest}};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"checkout">>, <<"status">>, CheckoutNr | Rest ], _Msg, Context) ->
    case checkout_status(CheckoutNr, Context) of
        {ok, Status} ->
            {ok, {Status, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"invoice">>, Id | Rest ], _Msg, Context) ->
    InvId = z_convert:to_integer(Id),
    Res = case is_allowed_paysub(Context) of
        true ->
            get_invoice(InvId, Context);
        false ->
            get_user_invoice(z_acl:user(Context), InvId, Context)
    end,
    case Res of
        {ok, Inv} ->
            {ok, {Inv, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"rsc">>, RscId, <<"invoices">>, <<"count">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    {ok, {count_user_invoices(RId, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"rsc">>, RscId, <<"invoices">>, <<"list">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            Res = case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    user_invoices(RId, Context);
                false ->
                    {error, eacces}
            end,
            case Res of
                {ok, Invs} ->
                    {ok, {Invs, Rest}};
                {error, _} = Error ->
                    Error
            end
    end;
m_get([ <<"subscription">>, Id | Rest ], _Msg, Context) ->
    SubId = z_convert:to_integer(Id),
    Res = case is_allowed_paysub(Context) of
        true ->
            get_subscription(SubId, Context);
        false ->
            get_user_subscription(z_acl:user(Context), SubId, Context)
    end,
    case Res of
        {ok, Sub} ->
            {ok, {Sub, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"rsc">>, RscId, <<"subscriptions">>, <<"count">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    {ok, {count_user_subscriptions(RId, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"rsc">>, RscId, <<"subscriptions">>, <<"list">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            Res = case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    user_subscriptions(RId, Context);
                false ->
                    {error, eacces}
            end,
            case Res of
                {ok, Subs} ->
                    {ok, {Subs, Rest}};
                {error, _} = Error ->
                    Error
            end
    end;
m_get([ <<"payment">>, Id | Rest ], _Msg, Context) ->
    PaymentId = z_convert:to_integer(Id),
    Res = case is_allowed_paysub(Context) of
        true ->
            get_payment(PaymentId, Context);
        false ->
            get_user_payment(z_acl:user(Context), PaymentId, Context)
    end,
    case Res of
        {ok, Sub} ->
            {ok, {Sub, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"rsc">>, RscId, <<"payments">>, <<"count">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    {ok, {count_user_payments(RId, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"rsc">>, RscId, <<"payments">>, <<"list">> | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        RId ->
            UserId = z_acl:user(Context),
            Res = case is_allowed_paysub(Context)
                orelse RId =:= UserId
                orelse is_user_maincontact(RscId, Context)
            of
                true ->
                    user_payments(RId, Context);
                false ->
                    {error, eacces}
            end,
            case Res of
                {ok, Subs} ->
                    {ok, {Subs, Rest}};
                {error, _} = Error ->
                    Error
            end
    end;
m_get([ <<"product">>, Id | Rest ], _Msg, Context) ->
    case is_allowed_paysub(Context) of
        true ->
            ProdId = z_convert:to_integer(Id),
            case get_product(ProdId, Context) of
                {ok, Prod} ->
                    {ok, {Prod, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;
m_get([ <<"prices">>, PSP | Rest ], _Msg, Context) ->
    case is_allowed_paysub(Context) orelse z_acl:is_allowed(use, mod_admin, Context) of
        true ->
            {ok, Prices} = list_prices(PSP, Context),
            {ok, {Prices, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"price_info">>, PSP, PriceId | Rest ], _Msg, Context) ->
    case get_price(PSP, PriceId, Context) of
        {ok, Price} ->
            Info = maps:with([
                    <<"is_active">>,
                    <<"name">>,
                    <<"currency">>,
                    <<"amount">>,
                    <<"is_recurring">>,
                    <<"recurring_period">>,
                    <<"is_use_maincontact">>,
                    <<"product_name">>,
                    <<"billing_scheme">>
                ], Price),
            {ok, {Info, Rest}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"overview_by">>, <<"user_group">> | Rest ], _Msg, Context) ->
    case is_allowed_paysub(Context) of
        true ->
            {ok, L} = overview_ug(Context),
            {ok, {L, Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"subscriptions">>, <<"user_groups">> | Rest ], _Msg, Context) ->
    {ok, {user_groups(Context), Rest}};
m_get([ <<"subscriptions">>, <<"states">> | Rest ], _Msg, _Context) ->
    {ok, {all_states(), Rest}};
m_get([ <<"subscriptions">>, <<"psps">> | Rest ], _Msg, Context) ->
    {ok, {subscription_psps(Context), Rest}};
m_get([ <<"subscriptions">>, <<"products">> | Rest ], _Msg, Context) ->
    {ok, {subscription_products(Context), Rest}};
m_get([ <<"subscriptions">>, <<"prices">> | Rest ], _Msg, Context) ->
    {ok, {subscription_prices(Context), Rest}};
m_get([ <<"has_moveable_maincontact_subs">>, RscId | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case z_acl:rsc_editable(Id, Context) of
                true ->
                    {ok, {has_moveable_maincontact_subs(Id, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"moveable_maincontact_psps">>, RscId | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case z_acl:rsc_editable(Id, Context) of
                true ->
                    {ok, {moveable_maincontact_psps(Id, Context), Rest}};
                false ->
                    {error, eacces}
            end
    end.

-spec subscription_products(Context) -> ByPSP when
    Context :: z:context(),
    ByPSP :: #{ PSP := list( Product ) },
    PSP :: binary(),
    Product :: map().
subscription_products(Context) ->
    {ok, Rows} = z_db:qmap("
        select distinct prod.*
        from paysub_subscription sub,
             paysub_subscription_item item,
             paysub_price price,
             paysub_product prod
        where item.subscription_id = sub.id
          and item.psp = price.psp
          and item.psp_price_id = price.psp_price_id
          and price.psp = prod.psp
          and price.psp_product_id = prod.psp_product_id
        order by prod.psp, prod.name
        ",
        Context),
    lists:foldr(
        fun(#{ <<"psp">> := PSP } = M, Acc) ->
            Curr = maps:get(PSP, Acc, []),
            Acc#{
                PSP => [ M | Curr ]
            }
        end,
        #{},
        Rows).


-spec subscription_prices(Context) -> ByPSP when
    Context :: z:context(),
    ByPSP :: #{ PSP := list( Price ) },
    PSP :: binary(),
    Price :: map().
subscription_prices(Context) ->
    {ok, Rows} = z_db:qmap("
        select distinct price.*, prod.name as prod_name, prod.id as prod_id
        from paysub_subscription sub,
             paysub_subscription_item item,
             paysub_price price,
             paysub_product prod
        where item.subscription_id = sub.id
          and item.psp = price.psp
          and item.psp_price_id = price.psp_price_id
          and price.psp = prod.psp
          and price.psp_product_id = prod.psp_product_id
        order by price.psp, prod.name, price.psp_price_id
        ",
        Context),
    lists:foldr(
        fun(#{ <<"psp">> := PSP } = M, Acc) ->
            Curr = maps:get(PSP, Acc, []),
            Acc#{
                PSP => [ M | Curr ]
            }
        end,
        #{},
        Rows).


-spec subscription_psps(Context) -> PSPs when
    Context :: z:context(),
    PSPs :: [ binary() ].
subscription_psps(Context) ->
    Rs = z_db:q("
        select distinct(psp)
        from paysub_subscription",
        Context),
    [ PSP || {PSP} <- Rs ].

-spec user_groups(Context) -> RscIds when
    Context :: z:context(),
    RscIds :: [ m_rsc:resource_id() ].
user_groups(Context) ->
    Rs = z_db:q("
        select distinct(user_group_id)
        from paysub_product
        where user_group_id is not null",
        Context),
    [ Id || {Id} <- Rs ].


-spec access_states(Context) -> States when
    Context :: z:context(),
    States :: [ binary() ].
access_states(Context) ->
    active_states() ++ pending_states(Context).

-spec pending_states(Context) -> States when
    Context :: z:context(),
    States :: [ binary() ].
pending_states(Context) ->
    case m_config:get_boolean(mod_paysub, is_unpaid_access, Context) of
        true ->
            [
                <<"incomplete_expired">>,
                <<"unpaid">>
            ];
        false ->
            []
    end.

-spec active_states() -> States when
    States :: [ binary() ].
active_states() ->
    [
        <<"incomplete">>,
        <<"trialing">>,
        <<"active">>,
        <<"past_due">>
    ].

-spec inactive_states(Context) -> States when
    Context :: z:context(),
    States :: [ binary() ].
inactive_states(Context) ->
    all_states() -- active_states() -- pending_states(Context).


-spec all_states() -> States when
    States :: [ binary() ].
all_states() ->
    [
        <<"incomplete">>,
        <<"incomplete_expired">>,
        <<"trialing">>,
        <<"active">>,
        <<"past_due">>,
        <<"canceled">>,
        <<"unpaid">>
    ].


%% @doc Return a map with the billing address and email of the person. The values are
%% unescaped for direct use in APIs.
-spec billing_address(RscId, Context) -> {ok, Address} | {error, Reason} when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Address :: #{ binary() => binary() },
    Reason :: term().
billing_address(RscId, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            Addr = case m_rsc:p_no_acl(Id, <<"billing_country">>, Context) of
                None when None =:= undefined; None =:= <<>> ->
                    #{
                        <<"street_1">> => m_rsc:p_no_acl(Id, <<"address_street_1">>, Context),
                        <<"street_2">> => m_rsc:p_no_acl(Id, <<"address_street_2">>, Context),
                        <<"city">> => m_rsc:p_no_acl(Id, <<"address_city">>, Context),
                        <<"postcode">> => m_rsc:p_no_acl(Id, <<"address_postcode">>, Context),
                        <<"state">> => m_rsc:p_no_acl(Id, <<"address_state">>, Context),
                        <<"country">> => m_rsc:p_no_acl(Id, <<"address_country">>, Context)
                    };
                Country ->
                    #{
                        <<"street_1">> => m_rsc:p_no_acl(Id, <<"billing_street_1">>, Context),
                        <<"street_2">> => m_rsc:p_no_acl(Id, <<"billing_street_2">>, Context),
                        <<"city">> => m_rsc:p_no_acl(Id, <<"billing_city">>, Context),
                        <<"postcode">> => m_rsc:p_no_acl(Id, <<"billing_postcode">>, Context),
                        <<"state">> => m_rsc:p_no_acl(Id, <<"billing_state">>, Context),
                        <<"country">> => Country
                    }
            end,
            Addr1 = Addr#{
                <<"email">> => billing_email(Id, Context),
                <<"name">> => m_rsc:p_no_acl(Id, <<"billing_name">>, Context),
                <<"phone">> => billing_phone(Id, Context)
            },
            Addr2 = maps:fold(
                fun
                    (K, undefined, Acc) -> Acc#{ K => <<>> };
                    (<<"email">>, V, Acc) -> Acc#{ <<"email">> => V };
                    (K, V, Acc) -> Acc#{ K => z_html:unescape(V) }
                end,
                #{},
                Addr1),
            {ok, Addr2}
    end.

billing_email(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"billing_email">>, Context) of
        undefined ->
            m_rsc:p_no_acl(Id, <<"email_raw">>, Context);
        Email ->
            z_html:unescape(Email)
    end.

billing_name(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"billing_name">>, Context) of
        undefined ->
            {Name, _} = z_template:render_to_iolist("_name.tpl", [ {id, Id} ], z_acl:sudo(Context)),
            z_string:trim(z_html:unescape(iolist_to_binary(Name)));
        Name ->
            z_html:unescape(Name)
    end.

billing_phone(Id, Context) ->
    case m_rsc:p_no_acl(Id, <<"billing_phone">>, Context) of
        undefined ->
            z_html:unescape(m_rsc:p_no_acl(Id, <<"phone">>, Context));
        Phone ->
            z_html:unescape(Phone)
    end.


%% @doc Sync the billing address from the customer record to resource. Copy the customer address
%% to the billing address if the normal address is set and different.
-spec sync_billing_to_rsc(PSP, CustId, Context) -> {ok, RscId} | {error, Reason} when
    PSP :: atom() | binary(),
    CustId :: binary() | m_rsc:resource_id(),
    Context :: z:context(),
    RscId :: m_rsc:resource_id(),
    Reason :: term().
sync_billing_to_rsc(PSP, CustId, Context) ->
    case get_customer(PSP, CustId, Context) of
        {ok, #{ <<"rsc_id">> := undefined }} ->
            {error, no_rsc_id};
        {ok, #{
            <<"rsc_id">> := Id,
            <<"address_country">> := CustCountry,
            <<"address_street_1">> := CustStreet1,
            <<"address_street_2">> := CustStreet2,
            <<"address_city">> := CustCity,
            <<"address_state">> := CustState,
            <<"address_postcode">> := CustPostcode,
            <<"email">> := CustEmail,
            <<"phone">> := CustPhone
        }} when is_binary(CustCountry), CustCountry =/= <<>> ->
            CustPhoneBin = z_convert:to_binary(CustPhone),
            CustAddr = #{
                <<"country">> => z_convert:to_binary(CustCountry),
                <<"street_1">> => z_convert:to_binary(CustStreet1),
                <<"street_2">> => z_convert:to_binary(CustStreet2),
                <<"city">> => z_convert:to_binary(CustCity),
                <<"state">> => z_convert:to_binary(CustState),
                <<"postcode">> => z_convert:to_binary(CustPostcode),
                <<"email">> => z_convert:to_binary(CustEmail),
                <<"phone">> => CustPhoneBin
            },
            case billing_address(Id, Context) of
                {ok, CustAddr} ->
                    ok;
                {ok, #{
                    <<"phone">> := BillingPhone
                }} ->
                    Update = case m_rsc:p_no_acl(Id, <<"address_country">>, Context) of
                        undefined ->
                            #{
                                <<"address_country">> => CustCountry,
                                <<"address_street_1">> => CustStreet1,
                                <<"address_street_2">> => CustStreet2,
                                <<"address_city">> => CustCity,
                                <<"address_state">> => CustState,
                                <<"address_postcode">> => CustPostcode
                            };
                        _OtherCountry ->
                            #{
                                <<"billing_country">> => CustCountry,
                                <<"billing_street_1">> => CustStreet1,
                                <<"billing_street_2">> => CustStreet2,
                                <<"billing_city">> => CustCity,
                                <<"billing_state">> => CustState,
                                <<"billing_postcode">> => CustPostcode
                            }
                    end,
                    Update1 = case m_rsc:p_no_acl(Id, <<"email_raw">>, Context) of
                        CustEmail ->
                            Update;
                        _ when CustEmail =/= <<>> ->
                            Update#{
                                <<"billing_email">> => CustEmail
                            };
                        _ ->
                            Update
                    end,
                    Update2 = case BillingPhone of
                        <<>> ->
                            Update1#{
                                <<"phone">> => CustPhone
                            };
                        _ ->
                            Update1
                    end,
                    m_rsc:update(Id, Update2, [ no_touch ], z_acl:sudo(Context));
                {error, _} = Error ->
                    Error
            end;
        {ok, #{ <<"rsc_id">> := Id, <<"email">> := Email }} when is_binary(Email), Email =/= <<>> ->
            case z_convert:to_binary(billing_email(Id, Context)) of
                Email ->
                    {ok, Id};
                _OtherEmail ->
                    Update = #{
                        <<"billing_email">> => Email
                    },
                    m_rsc:update(Id, Update, [ no_touch ], z_acl:sudo(Context))
            end;
        {ok, #{ <<"rsc_id">> := Id }} ->
            {ok, Id};
        {error, _} = Error ->
            Error
    end.


%% @doc Sync the billing address from the customer record to the psp. Copy the local billing address
%% to psp if the information at the psp is different. The copying is dony by scheduling a task
%% to run in the future. This also catches quick successive updates.
-spec sync_billing_to_psp(RscId, Context) -> ok | {error, Reason} when
    RscId :: m_rsc:resource(),
    Context :: z:context(),
    Reason :: term().
sync_billing_to_psp(RscId, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            sync_billing_to_psp(stripe, Id, Context)
    end.

%% @doc Sync the billing address from the customer record to the psp. Copy the local billing address
%% to psp if the information at the psp is different. The copying is dony by scheduling a task
%% to run in the future. This also catches quick successive updates.
-spec sync_billing_to_psp(PSP, CustId, Context) -> ok | {error, Reason} when
    PSP :: atom() | binary(),
    CustId :: binary() | m_rsc:resource_id(),
    Context :: z:context(),
    Reason :: term().
sync_billing_to_psp(PSP, CustId, Context) ->
    case get_customer(PSP, CustId, Context) of
        {ok, #{ <<"rsc_id">> := undefined }} ->
            {error, no_rsc_id};
        {ok, #{
            <<"psp">> := PSPBin,
            <<"psp_customer_id">> := PSPCustomerId,
            <<"rsc_id">> := Id,
            <<"name">> := CustName,
            <<"phone">> := CustPhone,
            <<"email">> := CustEmail,
            <<"address_country">> := CustCountry,
            <<"address_street_1">> := CustStreet1,
            <<"address_street_2">> := CustStreet2,
            <<"address_postcode">> := CustPostcode,
            <<"address_city">> := CustCity,
            <<"address_state">> := CustState
        }} when is_binary(CustCountry), CustCountry =/= <<>> ->
            CustAddr = #{
                <<"email">> => z_convert:to_binary(CustEmail),
                <<"name">> => z_convert:to_binary(CustName),
                <<"phone">> => z_convert:to_binary(CustPhone),
                <<"country">> => z_convert:to_binary(CustCountry),
                <<"street_1">> => z_convert:to_binary(CustStreet1),
                <<"street_2">> => z_convert:to_binary(CustStreet2),
                <<"postcode">> => z_convert:to_binary(CustPostcode),
                <<"city">> => z_convert:to_binary(CustCity),
                <<"state">> => z_convert:to_binary(CustState)
            },
            case billing_address(Id, Context) of
                {ok, CustAddr} ->
                    {ok, Id};
                {ok, _} ->
                    IdBin = z_convert:to_binary(Id),
                    TaskId = <<"paysub-to-psp-", PSPBin/binary, $-, IdBin/binary>>,
                    Args = [ PSPBin, PSPCustomerId ],
                    z_pivot_rsc:insert_task_after(1, ?MODULE, task_sync_billing_to_psp, TaskId, Args, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {ok, _} ->
            ok;
        {error, _} = Error ->
            Error
    end.

task_sync_billing_to_psp(<<"stripe">>, PSPCustomerId, Context) ->
    case paysub_stripe:update_billing_address(PSPCustomerId, Context) of
        ok ->
            ok;
        {error, retry} ->
            {delay, 600};
        {error, _} = Error ->
            Error
    end;
task_sync_billing_to_psp(PSP, PSPCustomerId, _Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"Paysub: ignored sync to psp">>,
        psp => PSP,
        psp_customer_id => PSPCustomerId
    }).


%% @doc Check if the given user is a subscriber (of any valid subscription). If the
%% user doesn't exist or is invisible then 'false' is returned. If the user is a main
%% contact then the subject is also checked.
-spec is_subscriber(Id, Context) -> boolean() when
    Id :: m_rsc:resource(),
    Context :: z:context().
is_subscriber(Id, Context) ->
    case m_rsc:rid(Id, Context) of
        undefined ->
            false;
        UserId ->
            case z_acl:rsc_visible(UserId, Context) of
                true ->
                    MainContactOf = m_edge:subjects(UserId, hasmaincontact, Context),
                    SubscriberIds = [ UserId | MainContactOf ],
                    States = pending_states(Context) ++ active_states(),
                    F = fun() ->
                        Count = z_db:q1("
                            select count(*)
                            from paysub_subscription
                            where rsc_id = any($1::int[])
                              and status = any($2)
                            ",
                            [ SubscriberIds, States ],
                            Context),
                        Count > 0
                    end,
                    z_depcache:memo(F, {paysub_is_sub, Id}, ?DAY, [ paysub | SubscriberIds ], Context);
                false ->
                    false
            end
    end.


%% @doc Check is the current user has ACL permission to use the mod_paysub.
-spec is_allowed_paysub(Context) -> boolean() when
    Context :: z:context().
is_allowed_paysub(Context) ->
    z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_paysub, Context).

%% @doc Check if the current user is a main contact of the resource.
-spec is_user_maincontact(SubjectId, Context) -> boolean() when
    SubjectId :: m_rsc:resource_id(),
    Context :: z:context().
is_user_maincontact(SubjectId, Context) ->
    HasMainContact = m_edge:objects(SubjectId, hasmaincontact, Context),
    lists:member(z_acl:user(Context), HasMainContact).


%% @doc Query the database for the overview of status per user group.
-spec overview_ug(Context) -> {ok, map()} | {error, term()} when
    Context :: z:context().
overview_ug(Context) ->
    UserGroups = z_db:q("
        select distinct(user_group_id)
        from paysub_product
        where user_group_id is not null",
        Context),
    UserGroupIds = filter_sort:sort([ Id || {Id} <- UserGroups ], <<"title">>, Context),
    StatusPerUG = z_db:q("
        select prod.user_group_id, sub.status, count(*)
        from paysub_product prod,
             paysub_price price,
             paysub_subscription_item item,
             paysub_subscription sub
        where sub.status <> 'canceled'
          and item.subscription_id = sub.id
          and item.psp = price.psp
          and item.psp_price_id = price.psp_price_id
          and price.psp = prod.psp
          and price.psp_product_id = prod.psp_product_id
          and prod.user_group_id is not null
        group by prod.user_group_id, sub.status
        ",
        Context),
    Grouped = overview_ug_1(StatusPerUG, #{}),
    Totals = overview_ug_2(StatusPerUG, #{ <<"total">> => 0 }),
    {ok, #{
        states => all_states() -- [ <<"canceled">> ],
        user_groups => UserGroupIds,
        status => Grouped,
        total => Totals
    }}.

overview_ug_1([], Acc) ->
    Acc;
overview_ug_1([{Id, Status, Count}|Rest], Acc) ->
    PerId = maps:get(Id, Acc, #{ <<"total">> => 0 }),
    PerId1 = PerId#{
        Status => Count,
        <<"total">> => maps:get(<<"total">>, PerId) + Count
    },
    Acc1 = Acc#{
        Id => PerId1
    },
    overview_ug_1(Rest, Acc1).

overview_ug_2([], Acc) ->
    Acc;
overview_ug_2([{_Id, Status, Count}|Rest], Acc) ->
    Acc1 = Acc#{
        Status => maps:get(Status, Acc, 0) + Count,
        <<"total">> => maps:get(<<"total">>, Acc) + Count
    },
    overview_ug_2(Rest, Acc1).



products_add_prices(Prods, Context) ->
    lists:map(
        fun(#{
            <<"psp">> := PSP,
            <<"psp_product_id">> := PspProdId
        } = Prod) ->
            {ok, Prices} = z_db:qmap_props("
                select *,
                      (select count(*)
                       from paysub_subscription_item item
                       where item.psp = p.psp
                         and item.psp_price_id  = p.psp_price_id) as count
                from paysub_price p
                where p.psp = $1
                  and p.psp_product_id = $2
                order by name
                ", [ PSP, PspProdId ], Context),
            Prod#{
                <<"prices">> => Prices
            }
        end,
        Prods).

get_product(ProdId, Context) ->
    case z_db:qmap_props_row("
        select *
        from paysub_product
        where id = $1", [ ProdId ], Context)
    of
        {ok, Prod} ->
            [Prod1] = products_add_prices([Prod], Context),
            {ok, Prod1};
        {error, _} = Error ->
            Error
    end.

%% @doc Get a product by PSP and PSP product id. Include all prices into the product.
-spec get_product(PSP, PspProdId, Context) -> {ok, Product} | {error, Reason} when
    PSP :: binary() | atom(),
    PspProdId :: binary(),
    Context :: z:context(),
    Product :: map(),
    Reason :: term().
get_product(PSP, PspProdId, Context) ->
    case z_db:qmap_props_row("
        select *
        from paysub_product
        where psp = $1
          and psp_product_id = $2", [ PSP, PspProdId ], Context)
    of
        {ok, Prod} ->
            [Prod1] = products_add_prices([Prod], Context),
            {ok, Prod1};
        {error, _} = Error ->
            Error
    end.


%% @doc Count all products
-spec count_products(Context) -> Count when
    Context :: z:context(),
    Count :: non_neg_integer().
count_products(Context) ->
    z_db:q1("select count(*) from paysub_product", Context).

%% @doc Count all payments
-spec count_payments(Context) -> Count when
    Context :: z:context(),
    Count :: non_neg_integer().
count_payments(Context) ->
    z_db:q1("select count(*) from paysub_payment", Context).

%% @doc Count all payments for an user
-spec count_user_payments(UserId, Context) -> Count when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Count :: non_neg_integer().
count_user_payments(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            0;
        RscId ->
            z_db:q1("
                select count(*)
                from paysub_payment p
                    join paysub_customer cust
                    on p.psp = cust.psp
                    and p.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $1
                ", [RscId], Context)
    end.


%% @doc Count all invoices
-spec count_invoices(Context) -> Count when
    Context :: z:context(),
    Count :: non_neg_integer().
count_invoices(Context) ->
    z_db:q1("select count(*) from paysub_invoice", Context).

%% @doc Count all invoices for an user
-spec count_user_invoices(UserId, Context) -> Count when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Count :: non_neg_integer().
count_user_invoices(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            0;
        RscId ->
            z_db:q1("
                select count(*)
                from paysub_invoice inv
                    join paysub_customer cust
                    on inv.psp = cust.psp
                    and inv.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $1
                ", [RscId], Context)
    end.


%% @doc Count all subscriptions
-spec count_subscriptions(Context) -> Count when
    Context :: z:context(),
    Count :: non_neg_integer().
count_subscriptions(Context) ->
    z_db:q1("select count(*) from paysub_subscription", Context).

%% @doc Count all subscriptions for an user
-spec count_user_subscriptions(UserId, Context) -> Count when
    UserId :: m_rsc:resource(),
    Context :: z:context(),
    Count :: non_neg_integer().
count_user_subscriptions(UserId, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            0;
        RscId ->
            z_db:q1("
                select count(*)
                from paysub_subscription
                where rsc_id = $1
                ", [RscId], Context)
    end.

%% @doc Return the list of user groups the user has subscriptions for.
%% Status must be one of the access states.
-spec user_groups( m_rsc:resource(), z:context() ) -> [ m_rsc:resource_id() ].
user_groups(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined -> [];
        UserId -> users_groups([UserId], Context)
    end.

%% @doc Return the list of user groups the users have subscriptions for.
%% This is used for users that are the main contact of other resources with possible subscriptions.
%% Status can be: incomplete, incomplete_expired, trialing, active, past_due, canceled, or unpaid
%% Only 'unpaid' and 'canceled' are considered inactive subscriptions.
-spec users_groups(UserIds, Context) -> UserGroupIds when
    UserIds :: [ m_rsc:resource_id() ],
    Context :: z:context(),
    UserGroupIds :: [ m_rsc:resource_id() ].
users_groups(UserIds, Context) ->
    F = fun() ->
        z_db:q("
            select distinct prod.user_group_id
            from paysub_product prod
                join paysub_price price
                    on price.psp_product_id = prod.psp_product_id
                    and price.psp = prod.psp
                join paysub_subscription_item item
                    on price.psp_price_id = item.psp_price_id
                    and price.psp = item.psp
                join paysub_subscription sub
                    on sub.id = item.subscription_id
                    and sub.psp = prod.psp
            where sub.status = any($2)
              and sub.rsc_id = any($1)
              and prod.user_group_id is not null
            ", [ UserIds, access_states(Context) ], Context)
    end,
    Key = {?FUNCTION_NAME, UserIds},
    Ids = z_depcache:memo(F, Key, ?HOUR, [ paysub | UserIds ], Context),
    [ Id || {Id} <- Ids ].

%% @doc Flush cached dependencies on the resource id and the main contacts of
%% the resource id. Called when subscriptions are updated.
-spec flush_rsc_id(RscId, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Context :: z:context().
flush_rsc_id(RscId, Context) ->
    z_depcache:flush(RscId, Context),
    MainContactOf = m_edge:objects(RscId, hasmaincontact, Context),
    lists:foreach(
        fun(MId) ->
            z_depcache:flush(MId, Context)
        end,
        MainContactOf),
    z_notifier:notify(#paysub_subscription_done{ rsc_id = RscId }, Context),
    ok.

%% @doc Return the complete list of subscriptions for a user. Sorted by descending
%% period start (newest subscriptions first).
-spec user_subscriptions(UserId, Context) -> {ok, list(map())} | {error, enoent} when
    UserId :: m_rsc:resource_id(),
    Context :: z:context().
user_subscriptions(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined ->
            {error, enoent};
        UserId ->
            {ok, Subs} = z_db:qmap_props("
                select *
                from paysub_subscription
                where rsc_id = $1
                order by period_start desc, created desc",
                [ UserId ],
                Context),
            Subs1 = subscription_add_prices(Subs, Context),
            {ok, Subs1}
    end.

subscription_add_prices(Subs, Context) ->
    lists:map(
        fun(#{ <<"id">> := Id } = Sub) ->
            {ok, Prices} = z_db:qmap_props("
                select *
                from paysub_subscription_item item
                    left join paysub_price price
                        on price.psp_price_id = item.psp_price_id
                        and price.psp = item.psp
                    left join paysub_product prod
                        on prod.psp = price.psp
                        and prod.psp_product_id = price.psp_product_id
                where item.subscription_id = $1
                ",
                [ Id ],
                Context),
            Sub#{
                <<"items">> => Prices
            }
        end,
        Subs).

user_customers(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined ->
            {error, enoent};
        UserId ->
            z_db:qmap_props("
                select *
                from paysub_customer
                where rsc_id = $1
                order by created desc",
                [ UserId ],
                Context)
    end.

user_invoices(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined ->
            {error, enoent};
        UserId ->
            z_db:qmap_props("
                select inv.*,
                       cust.id as customer_id,
                       cust.rsc_id as rsc_id,
                       cust.email as email
                from paysub_invoice inv
                    left join paysub_customer cust
                    on inv.psp = cust.psp
                    and inv.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $1
                order by inv.created desc, id desc",
                [ UserId ],
                Context)
    end.

get_invoice(InvoiceId, Context) ->
    z_db:qmap_props_row("
        select inv.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_invoice inv
            left join paysub_customer cust
            on inv.psp = cust.psp
            and inv.psp_customer_id = cust.psp_customer_id
        where inv.id = $1",
        [ InvoiceId ],
        Context).

get_user_invoice(UserId, InvoiceId, Context) ->
    z_db:qmap_props_row("
        select inv.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_invoice inv
            join paysub_customer cust
            on inv.psp = cust.psp
            and inv.psp_customer_id = cust.psp_customer_id
        where inv.id = $1
          and cust.rsc_id = $2",
        [ InvoiceId, UserId ],
        Context).


get_subscription(SubId, Context) ->
    case z_db:qmap_props_row("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
            on sub.psp = cust.psp
            and sub.psp_customer_id = cust.psp_customer_id
        where sub.id = $1",
        [ SubId ],
        Context)
    of
        {ok, Sub} ->
            [Sub1] = subscription_add_prices([Sub], Context),
            {ok, Sub1};
        {error, _} = Error ->
            Error
    end.

get_subscription(PSP, SubId, Context) ->
    case z_db:qmap_props_row("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
            on sub.psp = cust.psp
            and sub.psp_customer_id = cust.psp_customer_id
        where sub.psp = $1
          and sub.psp_subscription_id = $2",
        [ PSP, SubId ],
        Context)
    of
        {ok, Sub} ->
            [Sub1] = subscription_add_prices([Sub], Context),
            {ok, Sub1};
        {error, _} = Error ->
            Error
    end.

get_user_subscription(UserId, SubId, Context) ->
    case z_db:qmap_props_row("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
            on sub.psp = cust.psp
            and sub.psp_customer_id = cust.psp_customer_id
        where sub.id = $1
          and sub.rsc_id = $2
          and cust.rsc_id = $2",
        [ SubId, UserId ],
        Context)
    of
        {ok, Sub} ->
            [Sub1] = subscription_add_prices([Sub], Context),
            {ok, Sub1};
        {error, _} = Error ->
            Error
    end.

get_payment(PaymentId, Context) ->
    case z_db:qmap_props_row("
        select p.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email,
               cust.name as name
        from paysub_payment p
            left join paysub_customer cust
            on p.psp = cust.psp
            and p.psp_customer_id = cust.psp_customer_id
        where p.id = $1",
        [ PaymentId ],
        Context)
    of
        {ok, Sub} ->
            [Sub1] = subscription_add_prices([Sub], Context),
            {ok, Sub1};
        {error, _} = Error ->
            Error
    end.

get_user_payment(UserId, PaymentId, Context) ->
    case z_db:qmap_props_row("
        select p.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email,
               cust.name as name
        from paysub_payment p
            left join paysub_customer cust
            on p.psp = cust.psp
            and p.psp_customer_id = cust.psp_customer_id
        where p.id = $1
          and cust.rsc_id = $2",
        [ PaymentId, UserId ],
        Context)
    of
        {ok, Sub} ->
            [Sub1] = subscription_add_prices([Sub], Context),
            {ok, Sub1};
        {error, _} = Error ->
            Error
    end.

user_payments(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined ->
            {error, enoent};
        UserId ->
            z_db:qmap_props("
                select p.*,
                       cust.id as customer_id,
                       cust.rsc_id as rsc_id,
                       cust.email as email
                from paysub_payment p
                    join paysub_customer cust
                    on p.psp = cust.psp
                    and p.psp_customer_id = cust.psp_customer_id
                where p.rsc_id = $1
                order by p.created desc, id desc",
                [ UserId ],
                Context)
    end.

-spec checkout_status(CheckoutNr, Context) -> {ok, map()} | {error, term()} when
    CheckoutNr :: binary() | undefined,
    Context :: z:context().
checkout_status(undefined, _Context) ->
    {error, enoent};
checkout_status(CheckoutNr, Context) ->
    Result = case z_db:qmap_props_row("
        select status,
               payment_status,
               rsc_id,
               requestor_id,
               survey_id,
               survey_answer_id,
               props_json
        from paysub_checkout
        where nr = $1",
        [ CheckoutNr ], Context)
    of
        {ok, #{
                <<"rsc_id">> := RscId,
                <<"requestor_id">> := RequestorId,
                <<"survey_id">> := SurveyId,
                <<"survey_answer_id">> := SurveyResultId,
                <<"is_use_maincontact">> := IsUseMainContact,
                <<"args">> := Args
            } = CheckoutStatus} ->
            S = status(CheckoutStatus),
            UserId = case RequestorId of
                undefined -> RscId;
                _ -> RequestorId
            end,
            S1 = S#{
                <<"rsc_id">> => RscId,
                <<"requestor_id">> => RequestorId,
                <<"user_id">> => UserId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"is_use_maincontact">> => IsUseMainContact,
                <<"args">> => Args
            },
            {ok, S1};
        {error, _} = Error ->
            Error
    end,
    maybe_add_user_info(Result, Context).

status(#{ <<"status">> := <<"open">> }) ->
    #{
        <<"status">> => <<"open">>,
        <<"is_failed">> => false,
        <<"is_paid">> => false
    };
status(#{ <<"status">> := <<"complete">>, <<"payment_status">> := <<"paid">> }) ->
    #{
        <<"status">> => <<"complete">>,
        <<"is_failed">> => false,
        <<"is_paid">> => true
    };
status(#{ <<"status">> := <<"complete">>, <<"payment_status">> := <<"no_payment_required">> }) ->
    #{
        <<"status">> => <<"complete">>,
        <<"is_failed">> => false,
        <<"is_paid">> => true
    };
status(#{ <<"status">> := <<"complete">> }) ->
    #{
        <<"status">> => <<"complete">>,
        <<"is_failed">> => false,
        <<"is_paid">> => false
    };
status(#{ <<"status">> := <<"expired">> }) ->
    #{
        <<"status">> => <<"expired">>,
        <<"is_failed">> => true,
        <<"is_paid">> => false
    }.

maybe_add_user_info({ok, #{ <<"is_failed">> := false, <<"is_paid">> := true, <<"user_id">> := UserId } = R}, Context) ->
    UserInfo = m_identity:get_user_info(UserId, Context),
    {ok, R#{ <<"user_info">> => UserInfo }};
maybe_add_user_info(R, _Context) ->
    R.

-spec checkout_create(PSP, UserId, Mode, Props, Context) -> Result when
    PSP :: atom() | binary(),
    UserId :: undefined | m_rsc:resource_id(),
    Mode :: payment | subscription,
    Props :: #{ atom() => term() },
    Context :: z:context(),
    Result :: {ok, CheckoutNr} | {error, term()},
    CheckoutNr :: binary().
checkout_create(PSP, UserId, Mode, Props, Context) ->
    Nr = z_ids:id(32),
    case z_db:insert(
        paysub_checkout,
        Props#{
            rsc_id => UserId,
            psp => PSP,
            mode => Mode,
            nr => Nr,
            status => <<"open">>,           % open, complete, or expired.
            payment_status => <<"unpaid">>  % unpaid, paid, no_payment_required
        },
        Context)
    of
        {ok, _} ->
            {ok, Nr};
        {error, _} = Error ->
            Error
    end.

-spec checkout_update(PSP, CheckoutNr, UpdateProps, Context) -> Result when
    PSP :: atom() | binary(),
    CheckoutNr :: binary(),
    UpdateProps :: #{ atom() => term() },
    Context :: z:context(),
    Result :: ok | {error, enoent}.
checkout_update(PSP, CheckoutNr, UpdateProps, Context) ->
    case z_db:q1("
        select id
        from paysub_checkout
        where psp = $1
          and nr = $2
        ", [ PSP, CheckoutNr ],
        Context)
    of
        undefined ->
            {error, enoent};
        CheckoutId ->
            z_db:update(
                paysub_checkout,
                CheckoutId,
                UpdateProps#{ modified => calendar:universal_time() },
                Context)
    end.


-spec checkout_completed(PSP, CheckoutNr, Context) -> Result when
    PSP :: atom() | binary(),
    CheckoutNr :: binary(),
    Context :: z:context(),
    Result :: ok | {error, term()}.
checkout_completed(PSP, CheckoutNr, Context) ->
    case checkout_status(CheckoutNr, Context) of
        {ok, #{ <<"status">> := Status } = CheckoutStatus} ->
            PSPCustomerId = z_db:q1("
                select psp_customer_id
                from paysub_checkout
                where psp = $1
                  and nr = $2",
                [ PSP, CheckoutNr ],
                Context),
            Customer = case get_customer(PSP, PSPCustomerId, Context) of
                {error, _} -> undefined;
                {ok, Cust} -> Cust
            end,
            z_notifier:notify_sync(#paysub_checkout_done{
                    status = Status,
                    checkout_status = CheckoutStatus,
                    customer = Customer
                },
                Context),
            ok;
        {error, _} = Error ->
            Error
    end.

-spec is_survey_checkout(SurveyId, Context) -> boolean() when
    SurveyId :: m_rsc:resource_id(),
    Context :: z:context().
is_survey_checkout(SurveyId, Context) ->
    case z_db:q1("
        select id
        from paysub_checkout
        where survey_id = $1
        limit 1",
        [ SurveyId ],
        Context)
    of
        undefined -> false;
        _ -> true
    end.

-spec survey_checkout_status(SurveyId, AnswerId, Context) -> {ok, Checkout} | {error, Reason} when
    SurveyId :: m_rsc:resource_id(),
    AnswerId :: integer(),
    Context :: z:context(),
    Checkout :: map(),
    Reason :: term().
survey_checkout_status(SurveyId, AnswerId, Context) ->
    z_db:qmap_props_row("
        select *
        from paysub_checkout
        where survey_id = $1
          and survey_answer_id = $2
        ",
        [ SurveyId, AnswerId ],
        Context).


-spec get_customer(PSP, UserIdOrCustId, Context) -> Result when
    PSP :: atom() | binary(),
    UserIdOrCustId :: undefined | m_rsc:resource_id() | binary(),
    Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
get_customer(_PSP, undefined, _Context) ->
    {error, enoent};
get_customer(PSP, UserId, Context) when is_integer(UserId) ->
    case z_db:qmap_props("
        select *
        from paysub_customer
        where rsc_id = $1
          and psp = $2
        ", [ UserId, PSP ], Context)
    of
        {ok, [ Cust ]} ->
            {ok, Cust};
        {ok, []} ->
            {error, enoent};
        {ok, Cs} ->
            % Select the customer with the most running subscriptions
            % or the most recent invoice.
            CsId = z_db:q1("
                select cust.psp_customer_id as cust_id,
                       count(sub.id) as ct,
                       min(inv.modified) as dt
                from paysub_customer cust
                    left join paysub_subscription sub
                        on sub.psp = cust.psp
                        and sub.psp_customer_id = cust.psp_customer_id
                        and status = any($3)
                    left join paysub_invoice inv
                        on inv.psp = cust.psp
                        and inv.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $1
                  and cust.psp = $2
                group by cust_id
                order by ct desc, dt desc
                ", [ UserId, PSP, access_states(Context) ], Context),
            C = hd(lists:filter(fun(#{ <<"psp_customer_id">> := CId }) -> CId =:= CsId end, Cs)),
            {ok, C};
        {error, _} = Error ->
            Error
    end;
get_customer(PSP, CustId, Context) when is_binary(CustId) ->
    z_db:qmap_props_row("
        select *
        from paysub_customer
        where psp_customer_id = $1
          and psp = $2
        ", [ CustId, PSP ], Context).


%% @doc List all products for a PSP
-spec list_products(PSP, Context) -> {ok, list( map() )} when
    PSP :: atom() | binary(),
    Context :: z:context().
list_products(PSP, Context) ->
    z_db:qmap_props("
        select *
        from paysub_product
        where psp = $1",
        [ PSP ], Context).


%% @doc List all prices for a PSP
-spec list_prices(PSP, Context) -> {ok, list( map() )} when
    PSP :: atom() | binary(),
    Context :: z:context().
list_prices(PSP, Context) ->
    z_db:qmap_props("
        select price.*,
               prod.name as product_name,
               prod.is_active as is_product_active
        from paysub_price price
            left join paysub_product prod
            on prod.psp = price.psp
            and prod.psp_product_id = price.psp_product_id
        where price.psp = $1
        order by
            price.is_active and prod.is_active desc,
            product_name,
            price.name,
            price.psp_price_id",
        [ PSP ], Context).


%% @doc List all customers for a PSP
-spec list_customers(PSP, Context) -> {ok, list( map() )} when
    PSP :: atom() | binary(),
    Context :: z:context().
list_customers(PSP, Context) ->
    z_db:qmap_props("
        select *
        from paysub_customer
        where psp = $1",
        [ PSP ], Context).


%% @doc Place all found products in the database. Do not delete any products.
-spec sync_products(PSP, Products, Context) -> ok when
    PSP :: atom() | binary(),
    Products :: list( map() ),
    Context :: z:context().
sync_products(PSP, Products, Context) ->
    lists:foreach(
        fun(Prod) ->
            sync_product(PSP, Prod, Context)
        end,
        Products).

%% @doc Place a single product in the database.
-spec sync_product(PSP, Product, Context) -> ok when
    PSP :: atom() | binary(),
    Product :: map(),
    Context :: z:context().
sync_product(PSP, #{ psp_product_id := ProdId } = Prod, Context) ->
    case z_db:q1("
        select id
        from paysub_product
        where psp = $1
          and psp_product_id = $2",
        [ PSP, ProdId ],
        Context)
    of
        undefined ->
            Prod1 = Prod#{ psp => PSP },
            {ok, _} = z_db:insert(paysub_product, Prod1, Context);
        Id ->
            {ok, _} = z_db:update(paysub_product, Id, Prod, Context)
    end,
    ok.

-spec update_product(ProductId, Prod, Context) -> ok | {error, enoent} when
    ProductId :: integer(),
    Prod :: map(),
    Context :: z:context().
update_product(ProductId, Prod, Context) ->
    case z_db:update(paysub_product, ProductId, Prod, Context) of
        {ok, 1} ->
            ok;
        {ok, 0} ->
            {error, enoent};
        {error, _} = Error ->
            Error
    end.


%% @doc Delete a product.
-spec delete_product(PSP, ProdId, Context) -> ok  | {error, enoent} when
    PSP :: atom() | binary(),
    ProdId :: binary(),
    Context :: z:context().
delete_product(PSP, ProdId, Context) ->
    case z_db:q("
        delete from paysub_product
        where psp = $1
          and psp_product_id = $2
        ",
        [ PSP, ProdId ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.



%% @doc Place all found prices in the database. Do not delete any prices.
-spec sync_prices(PSP, Prices, Context) -> ok when
    PSP :: atom() | binary(),
    Prices :: list( map() ),
    Context :: z:context().
sync_prices(PSP, Prices, Context) ->
    lists:foreach(
        fun(Price) ->
            sync_price(PSP, Price, Context)
        end,
        Prices).

%% @doc Place all found prices in the database. Do not delete any prices.
-spec sync_price(PSP, Price, Context) -> ok when
    PSP :: atom() | binary(),
    Price :: map(),
    Context :: z:context().
sync_price(PSP, #{ psp_price_id := PriceId } = Price, Context) ->
    case z_db:q1("
        select id
        from paysub_price
        where psp = $1
          and psp_price_id = $2",
        [ PSP, PriceId ],
        Context)
    of
        undefined ->
            Price1 = Price#{ psp => PSP },
            {ok, _} = z_db:insert(paysub_price, Price1, Context);
        Id ->
            {ok, _} = z_db:update(paysub_price, Id, Price, Context)
    end,
    ok.


%% @doc Delete a price.
-spec delete_price(PSP, PriceId, Context) -> ok  | {error, enoent} when
    PSP :: atom() | binary(),
    PriceId :: binary(),
    Context :: z:context().
delete_price(PSP, PriceId, Context) ->
    case z_db:q("
        delete from paysub_price
        where psp = $1
          and psp_price_id = $2
        ",
        [ PSP, PriceId ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Get a price record. First check on the price key, then
%% on the price PSP id.
-spec get_price(PSP, PriceIdOrName, Context) -> Result when
    PSP :: atom() | binary(),
    PriceIdOrName :: undefined | binary(),
    Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
get_price(_PSP, undefined, _Context) ->
    {error, enoent};
get_price(_PSP, <<>>, _Context) ->
    {error, enoent};
get_price(PSP, PriceIdOrKey, Context) when is_binary(PriceIdOrKey) ->
    case z_db:qmap_props_row("
        select price.*,
               prod.is_use_maincontact,
               prod.name as product_name
        from paysub_price price
            left join paysub_product prod
            on prod.psp = price.psp
            and prod.psp_product_id = price.psp_product_id
        where price.key = $1
          and price.psp = $2
        ", [ PriceIdOrKey, PSP ], Context)
    of
        {ok, _} = Ok ->
            Ok;
        {error, enoent} ->
            z_db:qmap_props_row("
                select price.*,
                       prod.is_use_maincontact,
                       prod.name as product_name
                from paysub_price price
                    left join paysub_product prod
                    on prod.psp = price.psp
                    and prod.psp_product_id = price.psp_product_id
                where price.psp_price_id = $1
                  and price.psp = $2
                ", [ PriceIdOrKey, PSP ], Context)
    end.


%% @doc Place all found payment (intents) in the database. Do not delete any payment.
-spec sync_payments(PSP, Payments, Context) -> ok when
    PSP :: atom() | binary(),
    Payments :: list( map() ),
    Context :: z:context().
sync_payments(PSP, Payments, Context) ->
    lists:foreach(
        fun(Payment) ->
            sync_payment(PSP, Payment, Context)
        end,
        Payments).

%% @doc Place a found payment in the database.
-spec sync_payment(PSP, Payment, Context) -> ok when
    PSP :: atom() | binary(),
    Payment :: map(),
    Context :: z:context().
sync_payment(PSP, #{ psp_payment_id := PaymentId } = Payment, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"PaySub: Updating payment intent">>,
        psp => PSP,
        psp_payment_id => PaymentId
    }),
    z_db:transaction(
        fun(Ctx) ->
            sync_payment_trans(PSP, Payment, Ctx)
        end,
        Context).

sync_payment_trans(PSP, #{ psp_payment_id := PaymentId } = Payment, Context) ->
    case z_db:q1("
        select id
        from paysub_payment
        where psp = $1
          and psp_payment_id = $2
        for update",
        [ PSP, PaymentId ],
        Context)
    of
        undefined ->
            Payment1 = Payment#{ psp => PSP },
            case z_db:insert(paysub_payment, Payment1, Context) of
                {ok, _} ->
                    ok;
                {error, #error{ codename = unique_violation }} ->
                    Id = z_db:q1("
                        select id
                        from paysub_payment
                        where psp = $1
                          and psp_payment_id = $2
                        for update",
                        [ PSP, PaymentId ],
                        Context),
                {ok, _} = z_db:update(paysub_payment, Id, Payment, Context),
                ok
            end;
        Id ->
            {ok, _} = z_db:update(paysub_payment, Id, Payment, Context),
            ok
    end.


%% @doc Delete a payment.
-spec delete_payment(PSP, PaymentId, Context) -> ok  | {error, enoent} when
    PSP :: atom() | binary(),
    PaymentId :: binary(),
    Context :: z:context().
delete_payment(PSP, PaymentId, Context) ->
    case z_db:q("
        delete from paysub_payment
        where psp = $1
          and psp_payment_id = $2
        ",
        [ PSP, PaymentId ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Set the resource id of a customer record, its subscriptions and checkouts.
%% The rsc_id will be propagated to the PSP with an async task.
-spec update_customer_rsc_id(PSP, PspCustId, RscId, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    PspCustId :: binary(),
    RscId :: undefined | m_rsc:resource_id(),
    Context :: z:context().
update_customer_rsc_id(PSP, PspCustId, RscId, Context) ->
    z_db:q("
        update paysub_checkout
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2
          and ( rsc_id <> $3
              or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context),
    update_customer_rsc_id_1(PSP, PspCustId, RscId, Context).

%% @doc Set the resource id of a customer record, its subscriptions and checkouts.
%% The rsc_id will be propagated to the PSP with an async task.
-spec update_customer_rsc_id(PSP, PspCustId, RscId, RequestorId, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    PspCustId :: binary(),
    RscId :: undefined | m_rsc:resource_id(),
    RequestorId :: undefined | m_rsc:resource_id(),
    Context :: z:context().
update_customer_rsc_id(PSP, PspCustId, RscId, RequestorId, Context) ->
    z_db:q("
        update paysub_checkout
        set rsc_id = $3,
            requestor_id = $4
        where psp = $1
          and psp_customer_id = $2
          and ( rsc_id <> $3
              or rsc_id is null
              or requestor_id <> $4
              or requestor_id is null)",
        [ PSP, PspCustId, RscId, RequestorId ],
        Context),
    update_customer_rsc_id_1(PSP, PspCustId, RscId, Context).

%% @doc Set the resource id of a customer record, its subscriptions and checkouts.
%% The rsc_id will be propagated to the PSP with an async task.
-spec update_customer_rsc_id_1(PSP, PspCustId, RscId, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    PspCustId :: binary(),
    RscId :: undefined | m_rsc:resource_id(),
    Context :: z:context().
update_customer_rsc_id_1(PSP, PspCustId, RscId, Context) ->
    N = z_db:q("
        update paysub_subscription
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2
          and (rsc_id <> $3 or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context),
    flush_rsc_id(RscId, Context),
    % Updating the customer might need update of metadata at the PSP
    {Result, N2} = case z_db:q("
        update paysub_customer
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2
          and (rsc_id <> $3 or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context)
    of
        1 ->
            N1 = z_db:q("
                update paysub_subscription
                set rsc_id = $3
                where psp = $1
                  and psp_customer_id = $2",
                [ PSP, PspCustId, RscId ],
                Context),
            % Update the customer at the PSP async with retries.
            update_customer_psp(PSP, PspCustId, Context),
            {ok, N1};
        0 ->
            case z_db:q1("
                select id
                from paysub_customer
                where psp = $1
                  and psp_customer_id = $2",
                [ PSP, PspCustId ],
                Context)
            of
                undefined -> {{error, enoent}, 0};
                _Id -> {ok, 0}
            end
    end,
    if
        N + N2 > 0 ->
            maybe_provision_user_subscriptions_async(RscId, Context);
        true ->
            ok
    end,
    Result.

%% @doc Sync the rsc_id to the PSP with an async task. This is needed after
%% the rsc_id has been updated, for example after a merge of subscriptions.
-spec sync_customer_rsc_id(RscId, Context) -> ok | {error, enoent} when
    RscId :: undefined | m_rsc:resource_id(),
    Context :: z:context().
sync_customer_rsc_id(RscId, Context) ->
    case RscId =:= undefined orelse m_rsc:exists(RscId, Context) of
        true ->
            Rs = z_db:q("
                select psp, psp_customer_id
                from paysub_customer
                where rsc_id = $1",
                [ RscId ],
                Context),
            lists:foreach(
                fun({PSP, PspCustId}) ->
                    update_customer_psp(PSP, PspCustId, Context)
                end,
                Rs);
        false ->
            {error, enoent}
    end.

%% @doc Update the customer records at the PSP.
-spec update_customer_psp(PSP, PspCustId, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    PspCustId :: binary(),
    Context :: z:context().
update_customer_psp(PSP, PspCustId, Context) ->
    PSP1 = z_convert:to_binary(PSP),
    Args = [ PSP1, PspCustId ],
    Key = <<PSP1/binary, $:, PspCustId/binary>>,
    z_pivot_rsc:insert_task_after(60, ?MODULE, update_customer_psp_task, Key, Args, Context).

update_customer_psp_task(<<"stripe">>, PspCustId, Context) ->
    paysub_stripe:update_customer_task(PspCustId, Context);
update_customer_psp_task(_PSP, _PspCustId, _Context) ->
    ok.


%% @doc Place all found customers in the database. Do not delete any customers.
-spec sync_customers(PSP, Custs, Context) -> ok when
    PSP :: atom() | binary(),
    Custs :: list( map() ),
    Context :: z:context().
sync_customers(PSP, Custs, Context) ->
    lists:foreach(
        fun(#{ psp_customer_id := _CustId } = Cust) ->
            sync_customer(PSP, Cust, Context)
        end,
        Custs).

-spec sync_customer(PSP, Cust, Context) -> ok when
    PSP :: atom() | binary(),
    Cust :: map(),
    Context :: z:context().
sync_customer(PSP, #{ psp_customer_id := CustId } = Cust0, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"PaySub: Updating customer">>,
        psp => PSP,
        psp_customer_id => CustId
    }),
    Cust = fix_country(Cust0#{ psp => PSP }),
    case is_customer_changed(PSP, Cust, Context) of
        true ->
            case z_db:transaction(
                fun(Ctx) ->
                    sync_customer_trans(PSP, Cust, Ctx)
                end,
                Context)
            of
                {ok, Action} ->
                    {ok, New} = get_customer(PSP, CustId, Context),
                    z_notifier:first(
                        #paysub_customer{
                            action = Action,
                            customer = New
                        }, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        false ->
            ok
    end.

fix_country(#{ address_country := Country } = Cust) when is_binary(Country) ->
    Country1 = case l10n_country2iso:country2iso(Country) of
        undefined -> z_string:truncatechars(Country, 16, <<>>);
        Iso -> Iso
    end,
    Cust#{ address_country => Country1 };
fix_country(Cust) ->
    Cust.

is_customer_changed(PSP, #{ psp_customer_id := CustId } = Cust, Context) ->
    case get_customer(PSP, CustId, Context) of
        {ok, Current} ->
            maps:fold(
                fun
                    (_K, _V, true) -> true;
                    (K, V, false) -> maps:get(K, Current, undefined) =/= V
                end,
                false,
                Cust);
        {error, enoent} ->
            true
    end.

sync_customer_trans(PSP, #{ psp_customer_id := CustId } = Cust, Context) ->
    case z_db:q1("
        select id
        from paysub_customer
        where psp = $1
          and psp_customer_id = $2
        for update",
        [ PSP, CustId ],
        Context)
    of
        undefined ->
            case z_db:insert(paysub_customer, Cust, Context) of
                {ok, _} ->
                    {ok, new};
                {error, #error{ codename = unique_violation }} ->
                    Id = z_db:q1("
                        select id
                        from paysub_customer
                        where psp = $1
                          and psp_customer_id = $2
                        for update",
                        [ PSP, CustId ],
                        Context),
                    {ok, _} = z_db:update(paysub_customer, Id, Cust, Context),
                    {ok, update};
                {error, _} = Error ->
                    Error
            end;
        Id ->
            {ok, _} = z_db:update(paysub_customer, Id, Cust, Context),
            {ok, update}
    end.

-spec delete_customer(PSP, Id, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    Id :: binary(),
    Context :: z:context().
delete_customer(PSP, Id, Context) ->
    R = get_customer(PSP, Id, Context),
    case z_db:q("
        delete from paysub_customer
        where psp = $1
          and psp_customer_id = $2",
        [ PSP, Id ],
        Context)
    of
        1 ->
            case R of
                {ok, Cust} ->
                    z_notifier:notify(
                        #paysub_customer{
                            action = delete,
                            customer = Cust
                        }, Context);
                {error, _} ->
                    ok
            end,
            ok;
        0 ->
            {error, enoent}
    end.

%% @doc Automatically provision all unprovisioned subscriptions for the rsc iff
%% the configuration `mod_paysub.manual_provision_only` is not set.
%% This is called after new subscriptions are received for customers that have their
%% rsc_id set. Or after the rsc_id of a customer is set (and synced to the subscriptions).
%% The provision is done async, in case the user has a recent checkout session, the
%% automatic provision is delayed for 10 minutes, so that the checkout has some time to
%% complete the provisioning.
-spec maybe_provision_user_subscriptions_async(RscId, Context) -> ok when
    RscId :: m_rsc:resource_id() | undefined,
    Context :: z:context().
maybe_provision_user_subscriptions_async(undefined, _Context) ->
    ok;
maybe_provision_user_subscriptions_async(RscId, Context) ->
    case m_config:get_boolean(mod_paysub, manual_provision_only, Context) of
        true ->
            ok;
        false ->
            N = z_db:q1("
                select count(*)
                from paysub_subscription
                where is_provisioned = false
                  and rsc_id = $1",
                [ RscId ],
                Context),
            if
                N > 0 ->
                    % If there is an active checkout for this user then wait a bit, as the
                    % subscription is probably being added as a result of the checkout.
                    Delay = case is_user_recent_checkout(RscId, Context) of
                        true -> 10*60;
                        false -> undefined
                    end,
                    z_pivot_rsc:insert_task_after(Delay, ?MODULE, provision_user_subscriptions, RscId, [RscId], Context),
                    ok;
                N =:= 0 ->
                    ok
            end
    end.

is_user_recent_checkout(RscId, Context) ->
    Recent = z_datetime:prev_minute(calendar:universal_time(), 10),
    N1 = z_db:q1("
        select count(*)
        from paysub_checkout
        where rsc_id = $1
          and modified > $2",
        [ RscId, Recent ],
        Context),
    N1 > 0.

%% @doc Provision all subscriptions of a user, this can be used if subscriptions
%% are added outside of the checkout flow. This could be by means of the PSP API, merging
%% resources, or by manually adding a subscription at the PSP.
%% This is automatically called for all subscriptions that get a resource attached, unless
%% the configuration key `mod_paysub.manual_provision_only` is set,
-spec provision_user_subscriptions(UserId, Context) -> ok when
    UserId :: m_rsc:resource_id() | undefined,
    Context :: z:context().
provision_user_subscriptions(undefined, _Context) ->
    ok;
provision_user_subscriptions(UserId, Context) ->
    UnProvisioned = z_db:q("
        select psp, psp_subscription_id
        from paysub_subscription
        where is_provisioned = false
          and rsc_id = $1",
        [ UserId ],
        Context),
    lists:foreach(
        fun({PSP, PspSubId}) ->
            provision_subscription(PSP, PspSubId, undefined, Context)
        end,
        UnProvisioned).


%% @doc Provision a new subscription. The subscription is already added to
%% the database. Sends a notification that a new subscription has been
%% created. This is called at the end of a completed checkout.
-spec provision_subscription(PSP, PspSubId, CheckoutNr, Context) -> ok | {error, Reason} when
    PSP :: binary() | atom(),
    PspSubId :: binary(),
    CheckoutNr :: binary() | undefined,
    Context :: z:context(),
    Reason :: term().
provision_subscription(PSP, PspSubId, CheckoutNr, Context) ->
    z_db:transaction(
        fun(Ctx) ->
            case z_db:qmap_row("
                    select *
                    from paysub_subscription
                    where psp = $1
                      and psp_subscription_id = $2
                    for update
                ", [ PSP, PspSubId ], Ctx)
            of
                {ok, #{
                    <<"id">> := SubId,
                    <<"psp_customer_id">> := PspCustId,
                    <<"is_provisioned">> := false
                } = Sub} ->
                    {ok, Cust} = get_customer(PSP, PspCustId, Ctx),
                    [Sub1] = subscription_add_prices([Sub], Context),
                    Checkout = case checkout_status(CheckoutNr, Context) of
                        {ok, CheckoutStatus} -> CheckoutStatus;
                        {error, _} -> undefined
                    end,
                    z_notifier:first(
                        #paysub_subscription{
                            action = new,
                            status = maps:get(<<"status">>, Sub1),
                            subscription = Sub1,
                            customer = Cust,
                            checkout_status = Checkout
                        }, Ctx),
                    1 = z_db:q("
                        update paysub_subscription
                        set is_provisioned = true
                        where id = $1",
                        [ SubId ],
                        Ctx),
                    ok;
                {ok, #{ <<"is_provisioned">> := true }} ->
                    % Ignore
                    ok;
                {error, _} = Error ->
                    Error
            end
        end,
        Context).

-spec sync_subscription(PSP, Sub, Prices, Context) -> ok | {error, term()} when
    PSP :: atom() | binary(),
    Sub :: map(),
    Prices :: list(map()),
    Context :: z:context().
sync_subscription(PSP,  #{ psp_subscription_id := PspSubId } = Sub, Prices, Context) ->
    PspCustId = maps:get(psp_customer_id, Sub, undefined),
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"PaySub: Updating subscription">>,
        psp => PSP,
        psp_subscription_id => PspSubId,
        psp_customer_id => PspCustId
    }),
    case z_db:transaction(
        fun(Ctx) ->
            sync_subscription_trans(PSP, Sub, Prices, Ctx)
        end,
        Context)
    of
        {ok, SubId} ->
            notify_subscription(SubId, false, Context),
            ok;
        {error, _} = E ->
            E
    end.

-spec update_subscription(SubId, Sub, Context) -> ok | {error, term()} when
    SubId :: integer(),
    Sub :: map(),
    Context :: z:context().
update_subscription(SubId, Sub, Context) ->
    case z_db:update(paysub_subscription, SubId, Sub, Context) of
        {ok, _} ->
            notify_subscription(SubId, false, Context),
            ok;
        {error, _} = E ->
            E
    end.

notify_subscription(SubId, IsRetry, Context) ->
    case z_db:transaction(
        fun(Ctx) ->
            case z_db:qmap_props_row("
                select *
                from paysub_subscription
                where id = $1
                for update",
                [ SubId ],
                Context)
            of
                {ok, #{
                    <<"psp">> := PSP,
                    <<"is_provisioned">> := true,
                    <<"status">> := Status,
                    <<"psp_customer_id">> := PspCustId
                } = Subrecord} ->
                    [Sub] = subscription_add_prices([Subrecord], Ctx),
                    Action = case Status of
                        <<"incomplete_expired">> -> delete;
                        <<"unpaid">> -> delete;
                        <<"canceled">> -> delete;
                        _ -> update
                    end,
                    case get_customer(PSP, PspCustId, Ctx) of
                        {ok, Cust} ->
                            case z_notifier:first(
                                #paysub_subscription{
                                    action = Action,
                                    status = Status,
                                    subscription = Sub,
                                    customer = Cust,
                                    checkout_status = undefined
                                }, Ctx)
                            of
                                undefined -> ok;
                                ok -> ok;
                                {error, _} = Error -> Error
                            end;
                        {error, _} when IsRetry ->
                            ?LOG_NOTICE(#{
                                in => zotonic_mod_paysub,
                                text => <<"Subscription notifier will retry as customer missing">>,
                                result => error,
                                reason => enoent,
                                psp => PSP,
                                psp_subscription_id => maps:get(<<"psp_subscription_id">>, Sub),
                                psp_customer_id => PspCustId
                            }),
                            {error, retry};
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Subscription notifier canceled as customer missing">>,
                                result => error,
                                reason => Reason,
                                psp => PSP,
                                psp_subscription_id => maps:get(<<"psp_subscription_id">>, Sub),
                                psp_customer_id => PspCustId
                            }),
                            {error, Reason}
                    end;
                {ok, #{
                    <<"is_provisioned">> := false
                }} ->
                    % The subscription is not yet provisioned -- ignore
                    ok;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        in => zotonic_mod_paysub,
                        text => <<"Subscription notifier canceled as subscription missing">>,
                        result => error,
                        reason => Reason,
                        sub_id => SubId
                    }),
                    Error
            end
        end,
        Context)
    of
        ok ->
            {RscId, WasProvisioned} = z_db:q_row("
                select rsc_id, is_provisioned
                from paysub_subscription sub
                where sub.id = $1",
                [ SubId ],
                Context),
            flush_rsc_id(RscId, Context),
            case WasProvisioned of
                true -> ok;
                false when is_integer(RscId) ->
                    maybe_provision_user_subscriptions_async(RscId, Context);
                false -> ok
            end;
        {error, retry} when IsRetry ->
            % Could be a race condition where the customer record is in transit
            % whilst the subscription is created.
            timer:sleep(1000),
            notify_subscription(SubId, true, Context);
        {error, _} = Error ->
            Error
    end.

sync_subscription_trans(PSP, #{ psp_subscription_id := PspSubId } = Sub, NewPrices, Context) ->
    {ok, SubId} = update_subscription_1(PSP, PspSubId, Sub, Context),
    {ok, CurrentPrices} = z_db:qmap("
        select psp_price_id, psp_item_id, quantity
        from paysub_subscription_item
        where subscription_id = $1",
        [ SubId ],
        [ {keys, atom} ],
        Context),
    NewIds = [ P || #{ psp_item_id := P } <- NewPrices ],
    CurrentIds = [ P || #{ psp_item_id := P } <- CurrentPrices ],
    New = NewIds -- CurrentIds,
    Del = CurrentIds -- NewIds,
    lists:foreach(
        fun(ItemId) ->
            z_db:q("
                delete from paysub_subscription_item
                where subscription_id = $1
                  and psp_item_id = $2",
                [ SubId, ItemId ],
                Context)
        end,
        Del),
    lists:foreach(
        fun(#{ psp_item_id := ItemId, psp_price_id := PriceId } = Price) ->
            case lists:member(ItemId, New) of
                true ->
                    Quantity = maps:get(quantity, Price, 1),
                    z_db:q("
                        insert into paysub_subscription_item
                            (subscription_id, psp, psp_price_id, psp_item_id, quantity)
                        values
                            ($1, $2, $3, $4, $5)",
                        [ SubId, PSP, PriceId, ItemId, Quantity ],
                        Context);
                false ->
                    ok
            end
        end,
        NewPrices),
    lists:foreach(
        fun(#{ psp_item_id := ItemId, psp_price_id := PriceId } = Price) ->
            NewQuantity = maps:get(quantity, Price, 1),
            z_db:q("
                update paysub_subscription_item
                set quantity = $5,
                    psp_price_id = $4
                where subscription_id = $1
                  and psp = $2
                  and psp_item_id = $3
                  and (psp_price_id <> $4 or quantity <> $5)",
                [ SubId, PSP, ItemId, PriceId, NewQuantity ],
                Context)
        end,
        NewPrices),
    {ok, SubId}.

update_subscription_1(PSP, PspSubId, Sub, Context) ->
    PspCustId = maps:get(psp_customer_id, Sub, undefined),
    RscId = case maps:get(rsc_id, Sub, undefined) of
        undefined ->
            z_db:q1("
                select rsc_id
                from paysub_customer
                where psp = $1
                  and psp_customer_id = $2",
                [ PSP, PspCustId],
                Context);
        RId when is_integer(RId) ->
            RId
    end,
    SubId = z_db:q1("
        insert into paysub_subscription (
            rsc_id, psp, psp_subscription_id, psp_customer_id,
            status,
            period_start, period_end, cancel_at, canceled_at,
            started_at, ended_at, trial_start, trial_end,
            modified
        ) values (
            $1, $2, $3, $4,
            $5,
            $6, $7, $8, $9,
            $10, $11, $12, $13,
            $14
        )
        on conflict(psp, psp_subscription_id)
        do update
        set rsc_id = excluded.rsc_id,
            status = excluded.status,
            period_start = excluded.period_start,
            period_end = excluded.period_end,
            cancel_at = excluded.cancel_at,
            canceled_at = excluded.canceled_at,
            started_at = excluded.started_at,
            ended_at = excluded.ended_at,
            trial_start = excluded.trial_start,
            trial_end = excluded.trial_end,
            modified = excluded.modified
        returning id
        ",
        [
            RscId,
            PSP,
            PspSubId,
            PspCustId,
            maps:get(status, Sub),
            maps:get(period_start, Sub),
            maps:get(period_end, Sub),
            maps:get(cancel_at, Sub),
            maps:get(canceled_at, Sub),
            maps:get(started_at, Sub),
            maps:get(ended_at, Sub),
            maps:get(trial_start, Sub),
            maps:get(trial_end, Sub),
            calendar:universal_time()
        ],
        Context),
    {ok, SubId}.


-spec delete_subscription(PSP, PspSubId, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    PspSubId :: binary(),
    Context :: z:context().
delete_subscription(PSP, PspSubId, Context) ->
    RscId = z_db:q1("
        select rsc_id
        from paysub_subscription
        where psp = $1
          and psp_subscription_id = $2",
        [ PSP, PspSubId ],
        Context),
    case z_db:q("
        delete from paysub_subscription
        where psp = $1
          and psp_subscription_id = $2",
        [ PSP, PspSubId ],
        Context)
    of
        1 ->
            flush_rsc_id(RscId, Context),
            ok;
        0 ->
            {error, enoent}
    end.


%% @doc Place all found invoices in the database. Do not delete any invoices.
-spec sync_invoices(PSP, Invs, Context) -> ok when
    PSP :: atom() | binary(),
    Invs :: list( map() ),
    Context :: z:context().
sync_invoices(PSP, Invs, Context) ->
    lists:foreach(
        fun(Inv) ->
            sync_invoice(PSP, Inv, Context)
        end,
        Invs).

%% @doc Place a single invoice in the database.
-spec sync_invoice(PSP, Inv, Context) -> ok when
    PSP :: atom() | binary(),
    Inv :: map(),
    Context :: z:context().
sync_invoice(PSP, #{ psp_invoice_id := InvId } = Inv, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"PaySub: Updating invoice">>,
        psp => PSP,
        psp_invoice_id => InvId
    }),
    z_db:transaction(
        fun(Ctx) ->
            sync_invoice_trans(PSP, Inv, Ctx)
        end,
        Context).

sync_invoice_trans(PSP, #{ psp_invoice_id := InvId } = Inv, Context) ->
    case z_db:q1("
        select id
        from paysub_invoice
        where psp = $1
          and psp_invoice_id = $2
        for update",
        [ PSP, InvId ],
        Context)
    of
        undefined ->
            Inv1 = Inv#{ psp => PSP },
            case z_db:insert(paysub_invoice, Inv1, Context) of
                {ok, _} ->
                    ok;
                {error, #error{ codename = unique_violation }} ->
                    Id = z_db:q1("
                        select id
                        from paysub_invoice
                        where psp = $1
                          and psp_invoice_id = $2
                        for update",
                        [ PSP, InvId ],
                        Context),
                    {ok, _} = z_db:update(paysub_invoice, Id, Inv, Context),
                    ok;
                {error, _} = Error ->
                    Error
            end;
        Id ->
            {ok, _} = z_db:update(paysub_invoice, Id, Inv, Context),
            ok
    end.


%% @doc Delete an invoice.
-spec delete_invoice(PSP, InvId, Context) -> ok  | {error, enoent} when
    PSP :: atom() | binary(),
    InvId :: binary(),
    Context :: z:context().
delete_invoice(PSP, InvId, Context) ->
    case z_db:q("
        delete from paysub_invoice
        where psp = $1
          and psp_invoice_id = $2
        ",
        [ PSP, InvId ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.


%% @doc Merge two resources, all invoices and customers of Loser are added to Winner.
-spec rsc_merge(WinnerId, LoserId, Context) -> ok when
    WinnerId :: m_rsc:resource_id(),
    LoserId :: m_rsc:resource_id(),
    Context :: z:context().
rsc_merge(WinnerId, LoserId, Context) ->
    z_db:q("
        update paysub_checkout
        set rsc_id = $1
        where rsc_id = $2
        ", [ WinnerId, LoserId ], Context),
    z_db:q("
        update paysub_subscription
        set rsc_id = $1
        where rsc_id = $2
        ", [ WinnerId, LoserId ], Context),
    z_db:q("
        update paysub_customer
        set rsc_id = $1
        where rsc_id = $2
        ", [ WinnerId, LoserId ], Context),
    flush_rsc_id(LoserId, Context),
    flush_rsc_id(WinnerId, Context),
    ok.


%% @doc Merge subscriptions from one resource to another, only for .
-spec move_subscriptions(FromId, ToId, IsOnlyMainContact, Context) -> ok when
    FromId :: m_rsc:resource_id(),
    ToId :: m_rsc:resource_id(),
    IsOnlyMainContact :: boolean(),
    Context :: z:context().
move_subscriptions(FromId, ToId, false, Context) ->
    rsc_merge(ToId, FromId, Context);
move_subscriptions(FromId, ToId, true, Context) ->
    % Merge the data for the PSPs the user has a maincontact subscription for.
    PSPs = moveable_maincontact_psps(FromId, Context),
    lists:foreach(
        fun(PSP) ->
            z_db:q("
                update paysub_checkout
                set rsc_id = $1
                where rsc_id = $2
                  and psp = $3
                ", [ ToId, FromId, PSP ], Context),
            z_db:q("
                update paysub_subscription
                set rsc_id = $1
                where rsc_id = $2
                  and psp = $3
                ", [ ToId, FromId, PSP ], Context),
            z_db:q("
                update paysub_customer
                set rsc_id = $1
                where rsc_id = $2
                  and psp = $3
                ", [ ToId, FromId, PSP ], Context)
        end,
        PSPs),
    flush_rsc_id(FromId, Context),
    flush_rsc_id(ToId, Context),
    ok.

%% @doc Check if the resource has "main contact" subscriptions and is connected with
%% a hasmaincontact edge from another resource.
-spec has_moveable_maincontact_subs(Id, Context) -> boolean() when
    Id :: m_rsc:resource_id(),
    Context :: z:context().
has_moveable_maincontact_subs(Id, Context) ->
    case m_edge:subjects(Id, hasmaincontact, Context) of
        [] ->
            false;
        _ ->
            Count = z_db:q1("
                select count(*)
                from paysub_subscription sub
                    join paysub_subscription_item item
                        on item.subscription_id = sub.id
                    join paysub_price price
                        on  item.psp = price.psp
                        and item.psp_price_id = price.psp_price_id
                    join paysub_product prod
                        on prod.psp = price.psp
                        and prod.psp_product_id = price.psp_product_id
                where sub.rsc_id = $1
                  and prod.is_use_maincontact = true",
                [ Id ],
                Context),
            Count > 0
    end.

%% @doc Return the PSPs for which the resource has "main contact" subscriptions and is connected with
%% a hasmaincontact edge from another resource.
-spec moveable_maincontact_psps(Id, Context) -> PSPs when
    Id :: m_rsc:resource_id(),
    PSPs :: list(binary()),
    Context :: z:context().
moveable_maincontact_psps(Id, Context) ->
    case m_edge:subjects(Id, hasmaincontact, Context) of
        [] ->
            [];
        _ ->
            PSPs = z_db:q("
                select distinct sub.psp
                from paysub_subscription sub
                    join paysub_subscription_item item
                        on item.subscription_id = sub.id
                    join paysub_price price
                        on  item.psp = price.psp
                        and item.psp_price_id = price.psp_price_id
                    join paysub_product prod
                        on prod.psp = price.psp
                        and prod.psp_product_id = price.psp_product_id
                where sub.rsc_id = $1
                  and prod.is_use_maincontact = true",
                [ Id ],
                Context),
            [ PSP || {PSP} <- PSPs ]
    end.

