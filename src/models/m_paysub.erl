%% @copyright 2022-2023 Marc Worrell
%% @doc Model for paid subscriptions
%% @end

%% Stripe Subscription status model
%% ================================
%%
%% Possible values are: incomplete, incomplete_expired, trialing, active, past_due, canceled, or unpaid.
%%
%% For collection_method=charge_automatically a subscription moves into incomplete if the initial payment
%% attempt fails. A subscription in this state can only have metadata and default_source updated. Once
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


-module(m_paysub).

-export([
    m_get/3,

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

    search_query_term/2,
    search_query/4,

    count_invoices/1,
    count_subscriptions/1,
    count_payments/1,

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

    get_customer/3,

    get_subscription/2,
    get_subscription/3,

    list_products/2,
    list_prices/2,
    list_customers/2,

    sync_products/3,
    sync_product/3,
    update_product/3,
    delete_product/3,

    sync_prices/3,
    sync_price/3,
    delete_price/3,
    get_price/3,

    sync_payments/3,
    sync_payment/3,
    delete_payment/3,

    provision_subscription/4,
    sync_subscription/4,
    delete_subscription/3,

    update_customer_rsc_id/4,
    update_customer_psp/3,
    update_customer_psp_task/3,

    sync_customer/3,
    sync_customers/3,
    delete_customer/3,

    sync_invoice/3,
    sync_invoices/3,
    delete_invoice/3,

    rsc_merge/3,

    install/1,
    drop_tables/1,

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

m_get([ <<"is_subscriber">> ], _Msg, Context) ->
    IsSubscriber = is_subscriber(z_acl:user(Context), Context),
    {ok, {IsSubscriber, []}};
m_get([ <<"is_subscriber">>, Id | Rest ], _Msg, Context) ->
    IsSubscriber = is_subscriber(Id, Context),
    {ok, {IsSubscriber, Rest}};
m_get([ <<"is_customer_portal">>, <<"stripe">> | Rest ], _Msg, Context) ->
    {ok, {paysub_stripe:is_customer_portal(z_acl:user(Context), Context), Rest}};
m_get([ <<"customer_portal_session_url">>, <<"stripe">> | Rest ], _Msg, Context) ->
    case paysub_stripe:customer_portal_session_url(z_acl:user(Context), Context) of
        {ok, Url} ->
            {ok, {Url, Rest}};
        {error, _} = Error ->
            Error
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
m_get([ <<"price_info">>, PSP, PriceId | Rest ], _Msg, Context) ->
    case get_price(PSP, PriceId, Context) of
        {ok, Price} ->
            Info = maps:with([
                    <<"is_active">>,
                    <<"name">>,
                    <<"currency">>,
                    <<"amount">>,
                    <<"is_recurring">>,
                    <<"recurring_period">>
                ], Price),
            {ok, {Info, Rest}};
        {error, _} = Error ->
            Error
    end.


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
                undefined ->
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
    z_html:unescape(m_rsc:p_no_acl(Id, <<"phone">>, Context)).


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
            <<"address_country">> := CustCountry,
            <<"address_street_1">> := CustStreet1,
            <<"address_street_2">> := CustStreet2,
            <<"address_city">> := CustCity,
            <<"address_state">> := CustState,
            <<"address_postcode">> := CustPostcode,
            <<"email">> := CustEmail
        }} when is_binary(CustCountry), CustCountry =/= <<>> ->
            CustAddr = #{
                <<"country">> => z_convert:to_binary(CustCountry),
                <<"street_1">> => z_convert:to_binary(CustStreet1),
                <<"street_2">> => z_convert:to_binary(CustStreet2),
                <<"city">> => z_convert:to_binary(CustCity),
                <<"state">> => z_convert:to_binary(CustState),
                <<"postcode">> => z_convert:to_binary(CustPostcode),
                <<"email">> => z_convert:to_binary(CustEmail)
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
                    Count = z_db:q1("
                        select count(*)
                        from paysub_subscription
                        where rsc_id = any($1::int[])
                          and status in ('incomplete', 'trialing', 'active', 'past_due')
                        ",
                        [ SubscriberIds ],
                        Context),
                    Count > 0;
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


%% @doc Extra query terms. Query for subscribers.
-spec search_query_term(QueryTerm, Context) -> SqlTerm when
    QueryTerm :: #search_query_term{},
    Context :: z:context(),
    SqlTerm :: #search_sql_term{}.
search_query_term(#search_query_term{ term = <<"is_subscriber">>, arg = Arg }, Context) ->
    PredId = m_rsc:rid(hasmaincontact, Context),
    Select = if
        PredId =:= undefined ->
            % All subscribers
            <<" select sub.rsc_id
                from paysub_subscription sub
                where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
            ">>;
        is_integer(PredId) ->
            % All subscribers and their main contacts
            <<" select sub.rsc_id
                from paysub_subscription sub
                where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
                union
                select mce.object_id
                from paysub_subscription sub
                    join edge mce
                    on mce.subject_id = sub.rsc_id
                    and mce.predicate_id = ", (integer_to_binary(PredId))/binary, "
                where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
            ">>
    end,
    case z_convert:to_bool(Arg) of
        true ->
            #search_sql_term{
                where = [
                    <<"rsc.id in (", Select/binary, ")">>
                ]
            };
        false ->
            #search_sql_term{
                where = [
                    <<"rsc.id not in (", Select/binary, ")">>
                ]
            }
    end;
search_query_term(#search_sql_term{}, _Context) ->
    undefined.


%% @doc Perform queries, helpers for the observer_search_query in mod_paysub.erl
search_query(invoices, #{ rsc_id := undefined }, {Offset, Limit}, Context) ->
    {ok, Result} = z_db:qmap_props("
        select inv.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_invoice inv
            left join paysub_customer cust
            on inv.psp = cust.psp
            and inv.psp_customer_id = cust.psp_customer_id
        order by inv.created desc, id desc
        offset $1 limit $2
        ",
        [ Offset-1, Limit ],
        Context),
    #search_result{
        result = Result,
        total = count_invoices(Context),
        is_total_estimated = false
    };
search_query(invoices, #{ rsc_id := UserId }, {Offset, Limit}, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            #search_result{
                result = [],
                total = 0,
                is_total_estimated = false
            };
        RscId ->
            {ok, Result} = z_db:qmap_props("
                select inv.*,
                       cust.id as customer_id,
                       cust.rsc_id as rsc_id,
                       cust.email as email
                from paysub_invoice inv
                    join paysub_customer cust
                    on inv.psp = cust.psp
                    and inv.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $3
                order by inv.created desc, id desc
                offset $1 limit $2
                ",
                [ Offset-1, Limit, RscId ],
                Context),
            #search_result{
                result = Result,
                total = count_user_invoices(RscId, Context),
                is_total_estimated = false
            }
    end;
search_query(subscriptions, #{ product_id := ProdId }, {Offset, Limit}, Context) when
        ProdId =/= <<>>, ProdId =/= undefined ->
    ProdId1 = z_convert:to_integer(ProdId),
    {ok, Subs} = z_db:qmap_props("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
                on sub.psp = cust.psp
                and sub.psp_customer_id = cust.psp_customer_id
            join paysub_subscription_item item
                on item.subscription_id = sub.id
            join paysub_price price
                on item.psp = price.psp
                and item.psp_price_id = price.psp_price_id
            join paysub_product product
                on price.psp = product.psp
                and price.psp_product_id = product.psp_product_id
        where product.id = $3
        order by sub.started_at desc, id desc
        offset $1 limit $2
        ",
        [ Offset-1, Limit, ProdId1 ],
        Context),
    Subs1 = subscription_add_prices(Subs, Context),
    #search_result{
        result = Subs1,
        total = count_product_subscriptions(ProdId1, Context),
        is_total_estimated = false
    };
search_query(subscriptions, #{ price_id := PriceId }, {Offset, Limit}, Context) when
        PriceId =/= <<>>, PriceId =/= undefined ->
    PriceId1 = z_convert:to_integer(PriceId),
    {ok, Subs} = z_db:qmap_props("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
                on sub.psp = cust.psp
                and sub.psp_customer_id = cust.psp_customer_id
            join paysub_subscription_item item
                on item.subscription_id = sub.id
            join paysub_price price
                on item.psp = price.psp
                and item.psp_price_id = price.psp_price_id
        where price.id = $3
        order by sub.started_at desc, id desc
        offset $1 limit $2
        ",
        [ Offset-1, Limit, PriceId1 ],
        Context),
    Subs1 = subscription_add_prices(Subs, Context),
    #search_result{
        result = Subs1,
        total = count_price_subscriptions(PriceId1, Context),
        is_total_estimated = false
    };
search_query(subscriptions, #{ rsc_id := undefined }, {Offset, Limit}, Context) ->
    {ok, Subs} = z_db:qmap_props("
        select sub.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as email
        from paysub_subscription sub
            left join paysub_customer cust
                on sub.psp = cust.psp
                and sub.psp_customer_id = cust.psp_customer_id
        order by sub.period_start desc, id desc
        offset $1 limit $2
        ",
        [ Offset-1, Limit ],
        Context),
    Subs1 = subscription_add_prices(Subs, Context),
    #search_result{
        result = Subs1,
        total = count_subscriptions(Context),
        is_total_estimated = false
    };
search_query(subscriptions, #{ rsc_id := UserId }, {Offset, Limit}, Context) ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            #search_result{
                result = [],
                total = 0,
                is_total_estimated = false
            };
        RscId ->
            {ok, Subs} = z_db:qmap_props("
                select sub.*,
                       cust.id as customer_id,
                       cust.rsc_id as rsc_id,
                       cust.email as email
                from paysub_subscription sub
                    left join paysub_customer cust
                    on sub.psp = cust.psp
                    and sub.psp_customer_id = cust.psp_customer_id
                where sub.rsc_id = $3
                order by sub.started_at desc, id desc
                offset $1 limit $2
                ",
                [ Offset-1, Limit, RscId ],
                Context),
            Subs1 = subscription_add_prices(Subs, Context),
            #search_result{
                result = Subs1,
                total = count_user_subscriptions(RscId, Context),
                is_total_estimated = false
            }
    end;
search_query(products, #{}, {Offset, Limit}, Context) ->
    {ok, Prods} = z_db:qmap_props("
        select *
        from paysub_product
        order by is_active desc, psp, name
        offset $1 limit $2
        ",
        [ Offset-1, Limit ],
        Context),
    Prods1 = products_add_prices(Prods, Context),
    #search_result{
        result = Prods1,
        total = count_products(Context),
        is_total_estimated = false
    };
search_query(payments, #{ rsc_id := UserId }, {Offset, Limit}, Context) when UserId =/= undefined ->
    case m_rsc:rid(UserId, Context) of
        undefined ->
            #search_result{
                result = [],
                total = 0,
                is_total_estimated = false
            };
        RscId ->
            {ok, Result} = z_db:qmap_props("
                select p.*,
                       cust.id as customer_id,
                       cust.rsc_id as rsc_id,
                       cust.email as customer_email,
                       cust.name as customer_name
                from paysub_payment p
                    join paysub_customer cust
                    on p.psp = cust.psp
                    and p.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $3
                order by p.created desc, id desc
                offset $1 limit $2
                ",
                [ Offset-1, Limit, RscId ],
                Context),
            #search_result{
                result = Result,
                total = count_user_payments(RscId, Context),
                is_total_estimated = false
            }
    end;
search_query(payments, #{}, {Offset, Limit}, Context) ->
    {ok, Result} = z_db:qmap_props("
        select p.*,
               cust.id as customer_id,
               cust.rsc_id as rsc_id,
               cust.email as customer_email,
               cust.name as customer_name
        from paysub_payment p
            left join paysub_customer cust
            on p.psp = cust.psp
            and p.psp_customer_id = cust.psp_customer_id
        order by p.created desc, id desc
        offset $1 limit $2
        ",
        [ Offset-1, Limit ],
        Context),
    #search_result{
        result = Result,
        total = count_payments(Context),
        is_total_estimated = false
    }.

%% @doc Count all products
-spec count_products(Context) -> Count when
    Context :: z:context(),
    Count :: non_neg_integer().
count_products(Context) ->
    z_db:q1("select count(*) from paysub_product", Context).


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

%% @doc Count all subscriptions with a certain price
-spec count_price_subscriptions(PriceId, Context) -> Count when
    PriceId :: integer(),
    Context :: z:context(),
    Count :: non_neg_integer().
count_price_subscriptions(PriceId, Context) ->
    z_db:q1("
        select count(*)
        from paysub_subscription_item item
            join paysub_price price
                on price.psp = item.psp
                and price.psp_price_id = item.psp_price_id
        where price.id = $1", [ PriceId ], Context).

%% @doc Count all subscriptions with a certain product
-spec count_product_subscriptions(ProductId, Context) -> Count when
    ProductId :: integer(),
    Context :: z:context(),
    Count :: non_neg_integer().
count_product_subscriptions(ProductId, Context) ->
    z_db:q1("
        select count(*)
        from paysub_subscription_item item
            join paysub_price price
                on price.psp = item.psp
                and price.psp_price_id = item.psp_price_id
            join paysub_product product
                on price.psp = product.psp
                and price.psp_product_id = product.psp_product_id
        where product.id = $1", [ ProductId ], Context).



%% @doc Return the list of user groups the user has subscriptions for.
%% Status can be: incomplete, incomplete_expired, trialing, active, past_due, canceled, or unpaid
%% Only 'unpaid' and 'canceled' are considered inactive subscriptions.
-spec user_groups( m_rsc:resource_id(), z:context() ) -> [ m_rsc:resource_id() ].
user_groups(UserId0, Context) ->
    case m_rsc:rid(UserId0, Context) of
        undefined ->
            [];
        UserId ->
            Ids = z_db:q("
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
                where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
                  and sub.rsc_id = $1
                  and prod.user_group_id is not null
                ", [ UserId ], Context),
            [ Id || {Id} <- Ids ]
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
    Ids = z_db:q("
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
        where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
          and sub.rsc_id = any($1)
          and prod.user_group_id is not null
        ", [ UserIds ], Context),
    [ Id || {Id} <- Ids ].


%% @doc Return the complete list of subscriptions for an user.
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
        fun(#{ <<"id">> := Id, <<"psp">> := PSP } = Sub) ->
            {ok, Prices} = z_db:qmap_props("
                select *
                from paysub_price price
                    left join paysub_subscription_item item
                        on price.psp_price_id = item.psp_price_id
                        and price.psp = item.psp
                    left join paysub_product prod
                        on prod.psp = price.psp
                        and prod.psp_product_id = price.psp_product_id
                where item.subscription_id = $2
                  and price.psp = $1
                ",
                [ PSP, Id ],
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
          and sub.psp_customer_id = $2",
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
    CheckoutNr :: binary(),
    Context :: z:context().
checkout_status(CheckoutNr, Context) ->
    Result = case z_db:qmap_props_row("
        select status, payment_status, rsc_id, survey_id, survey_answer_id, props_json
        from paysub_checkout
        where nr = $1",
        [ CheckoutNr ], Context)
    of
        {ok, #{
            <<"status">> := <<"open">>,
            <<"rsc_id">> := RscId,
            <<"survey_id">> := SurveyId,
            <<"survey_answer_id">> := SurveyResultId,
            <<"args">> := Args
        }} ->
            {ok, #{
                <<"status">> => <<"open">>,
                <<"is_failed">> => false,
                <<"is_paid">> => false,
                <<"user_id">> => RscId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"args">> => Args
            }};
        {ok, #{
            <<"status">> := <<"complete">>,
            <<"payment_status">> := <<"paid">>,
            <<"survey_id">> := SurveyId,
            <<"survey_answer_id">> := SurveyResultId,
            <<"rsc_id">> := RscId,
            <<"args">> := Args
        }} ->
            {ok, #{
                <<"status">> => <<"complete">>,
                <<"is_failed">> => false,
                <<"is_paid">> => true,
                <<"user_id">> => RscId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"args">> => Args
            }};
        {ok, #{
            <<"status">> := <<"complete">>,
            <<"payment_status">> := <<"no_payment_required">>,
            <<"survey_id">> := SurveyId,
            <<"survey_answer_id">> := SurveyResultId,
            <<"rsc_id">> := RscId,
            <<"args">> := Args
        }} ->
            {ok, #{
                <<"status">> => <<"complete">>,
                <<"is_failed">> => false,
                <<"is_paid">> => true,
                <<"user_id">> => RscId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"args">> => Args
            }};
        {ok, #{
            <<"status">> := <<"complete">>,
            <<"rsc_id">> := RscId,
            <<"survey_id">> := SurveyId,
            <<"survey_answer_id">> := SurveyResultId,
            <<"args">> := Args
        }} ->
            {ok, #{
                <<"status">> => <<"complete">>,
                <<"is_failed">> => false,
                <<"is_paid">> => false,
                <<"user_id">> => RscId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"args">> => Args
            }};
        {ok, #{
            <<"status">> := <<"expired">>,
            <<"rsc_id">> := RscId,
            <<"survey_id">> := SurveyId,
            <<"survey_answer_id">> := SurveyResultId,
            <<"args">> := Args
        }} ->
            {ok, #{
                <<"status">> => <<"expired">>,
                <<"is_failed">> => true,
                <<"is_paid">> => false,
                <<"user_id">> => RscId,
                <<"survey_id">> => SurveyId,
                <<"survey_answer_id">> => SurveyResultId,
                <<"args">> => Args
            }};
        {error, _} = Error ->
            Error
    end,
    maybe_add_user_info(Result, Context).

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
                        and status in ('incomplete', 'trialing', 'active', 'past_due')
                    left join paysub_invoice inv
                        on inv.psp = cust.psp
                        and inv.psp_customer_id = cust.psp_customer_id
                where cust.rsc_id = $1
                  and cust.psp = $2
                group by cust_id
                order by ct desc, dt desc
                ", [ UserId, PSP ], Context),
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
        select *
        from paysub_price
        where psp = $1",
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

%% @doc Get a price record.
-spec get_price(PSP, PriceIdOrName, Context) -> Result when
    PSP :: atom() | binary(),
    PriceIdOrName :: undefined | binary(),
    Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
get_price(_PSP, undefined, _Context) ->
    {error, enoent};
get_price(PSP, PriceIdOrName, Context) when is_binary(PriceIdOrName) ->
    z_db:qmap_props_row("
        select *
        from paysub_price
        where (psp_price_id = $1 or name = $1)
          and psp = $2
        ", [ PriceIdOrName, PSP ], Context).


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
          and (rsc_id <> $3 or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context),
    z_db:q("
        update paysub_subscription
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2
          and (rsc_id <> $3 or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context),
    % Updating the customer might need update of metadata at the PSP
    case z_db:q("
        update paysub_customer
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2
          and (rsc_id <> $3 or rsc_id is null)",
        [ PSP, PspCustId, RscId ],
        Context)
    of
        1 ->
            z_db:q("
                update paysub_subscription
                set rsc_id = $3
                where psp = $1
                  and psp_customer_id = $2",
                [ PSP, PspCustId, RscId ],
                Context),
            % Update the customer at the PSP async with retries.
            update_customer_psp(PSP, PspCustId, Context),
            ok;
        0 ->
            case z_db:q("
                select id
                from paysub_customer
                where psp = $1
                  and psp_customer_id = $2",
                [ PSP, PspCustId ],
                Context)
            of
                undefined -> {error, enoent};
                _Id -> ok
            end
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
sync_customer(PSP, #{ psp_customer_id := CustId } = Cust, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"PaySub: Updating customer">>,
        psp => PSP,
        psp_customer_id => CustId
    }),
    z_db:transaction(
        fun(Ctx) ->
            sync_customer_trans(PSP, Cust, Ctx)
        end,
        Context).

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
            Cust1 = Cust#{ psp => PSP },
            case z_db:insert(paysub_customer, Cust1, Context) of
                {ok, _} ->
                    ok;
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
                    ok;
                {error, _} = Error ->
                    Error
            end;
        Id ->
            {ok, _} = z_db:update(paysub_customer, Id, Cust, Context),
            ok
    end.

-spec delete_customer(PSP, Id, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    Id :: binary(),
    Context :: z:context().
delete_customer(PSP, Id, Context) ->
    case z_db:q("
        delete from paysub_customer
        where psp = $1
          and psp_customer_id = $2",
        [ PSP, Id ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

%% @doc Provision a new subscription. The subscription is already added to
%% the database. Sends a notification that a new subscription has been
%% created.
-spec provision_subscription(PSP, PspSubId, CheckoutNr, Context) -> ok | {error, Reason} when
    PSP :: binary() | atom(),
    PspSubId :: binary(),
    CheckoutNr :: binary(),
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
                        {error, _} -> #{}
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

-spec sync_subscription(PSP, Sub, PriceIds, Context) -> ok when
    PSP :: atom() | binary(),
    Sub :: map(),
    PriceIds :: list( binary() ),
    Context :: z:context().
sync_subscription(PSP,  #{ psp_subscription_id := PspSubId } = Sub, PriceIds, Context) ->
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
            sync_subscription_trans(PSP, Sub, PriceIds, Ctx)
        end,
        Context)
    of
        {ok, SubId} ->
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
                            Result = z_notifier:first(
                                #paysub_subscription{
                                    action = Action,
                                    status = Status,
                                    subscription = Sub,
                                    customer = Cust,
                                    checkout_status = undefined
                                }, Ctx),
                            Result;
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
            ok;
        {error, retry} when IsRetry ->
            % Could be a race condition where the customer record is in transit
            % whilst the subscription is created.
            timer:sleep(1000),
            notify_subscription(SubId, true, Context);
        {error, _} = Error ->
            Error
    end.

sync_subscription_trans(PSP, #{ psp_subscription_id := PspSubId } = Sub, PriceIds, Context) ->
    {ok, SubId} = update_subscription_1(PSP, PspSubId, Sub, Context),
    Current = z_db:q("
        select psp_price_id
        from paysub_subscription_item
        where subscription_id = $1",
        [ SubId ],
        Context),
    Current1 = [ P || {P} <- Current ],
    New = PriceIds -- Current1,
    Del = Current1 -- PriceIds,
    lists:foreach(
        fun(PriceId) ->
            z_db:q("
                delete from paysub_subscription_item
                where subscription_id = $1
                  and psp_price_id = $3",
                [ SubId, PriceId ],
                Context)
        end,
        Del),
    lists:foreach(
        fun(PriceId) ->
            z_db:q("
                insert into paysub_subscription_item
                    (subscription_id, psp, psp_price_id)
                values
                    ($1, $2, $3)",
                [ SubId, PSP, PriceId ],
                Context)
        end,
        New),
    {ok, SubId}.

update_subscription_1(PSP, PspSubId, Sub, Context) ->
    case z_db:q1("
        select id
        from paysub_subscription
        where psp = $1
          and psp_subscription_id = $2
        for update",
        [ PSP, PspSubId ],
        Context)
    of
        undefined ->
            Sub1 = Sub#{ psp => PSP, modified => calendar:universal_time() },
            case z_db:insert(paysub_subscription, Sub1, Context) of
                {ok, _} = Ok ->
                    Ok;
                {error, #error{ codename = unique_violation }} ->
                    Id = z_db:q1("
                        select id
                        from paysub_subscription
                        where psp = $1
                          and psp_subscription_id = $2
                        for update",
                        [ PSP, PspSubId ],
                        Context),
                {ok, _} = z_db:update(paysub_subscription, Id, Sub1, Context),
                {ok, Id}
            end;
        Id ->
            Sub1 = Sub#{ modified => calendar:universal_time() },
            {ok, _} = z_db:update(paysub_subscription, Id, Sub1, Context),
            {ok, Id}
    end.


-spec delete_subscription(PSP, Id, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    Id :: binary(),
    Context :: z:context().
delete_subscription(PSP, Id, Context) ->
    case z_db:q("
        delete from paysub_subscription
        where psp = $1
          and psp_subscription_id = $2",
        [ PSP, Id ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
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
    ok.


-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(paysub_checkout, Context) of
        false ->
            ok = install_tables(Context),
            z_db:flush(Context),
            ok;
        true ->
            case z_db:table_exists(paysub_payment, Context) of
                false ->
                    [] = z_db:q("
                        create table paysub_payment (
                            id serial not null,
                            psp character varying(32) not null,
                            psp_payment_id character varying(128) not null,
                            psp_customer_id character varying(128),
                            psp_invoice_id character varying(128),
                            currency character varying(16) not null default 'EUR'::character varying,
                            amount integer not null,
                            amount_received integer not null,

                            name character varying(255),
                            email character varying(255),
                            phone character varying(255),

                            status character varying(32) not null default ''::character varying,
                            description character varying (300),

                            props_json jsonb,
                            created timestamp with time zone NOT NULL DEFAULT now(),
                            modified timestamp with time zone NOT NULL DEFAULT now(),

                            constraint paysub_payment_pkey primary key (id),
                            constraint paysub_payment_psp_payment_intent_id unique (psp, psp_payment_id)
                        )", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_psp_customer_key ON paysub_payment (psp, psp_customer_id)", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_modified_key ON paysub_payment (modified)", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_created_key ON paysub_payment (created)", Context),
                    z_db:flush(Context);
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_customer, phone, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_customer
                            add column address_street_1 character varying(255),
                            add column address_street_2 character varying(255),
                            add column address_city character varying(255),
                            add column address_state character varying(255),
                            add column address_postcode character varying(255),
                            add column phone character varying(255)
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_subscription, is_provisioned, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_subscription
                            add column is_provisioned boolean not null default false
                        ", Context),
                    z_db:flush(Context),
                    z_db:q("update paysub_subscription set is_provisioned = true", Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, psp_customer_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column psp_customer_id character varying(128)
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, survey_answer_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column survey_answer_id bigint
                        ", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_checkout_psp_psp_customer_id_key ON paysub_checkout (psp, psp_customer_id)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, survey_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column survey_id integer
                        ", Context),
                    [] = z_db:q("CREATE INDEX paysub_checkout_survey_id_key ON paysub_checkout (survey_id, survey_answer_id)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end
    end.

install_tables(Context) ->
    [] = z_db:q("
        create table paysub_checkout (
            id serial not null,
            rsc_id integer,
            survey_id integer,
            survey_answer_id bigint,
            nr character varying(32) not null,
            psp character varying(32) not null,
            psp_checkout_id character varying(128),
            psp_customer_id character varying(128),
            mode character varying(32) not null,
            status character varying(32) not null default 'open'::character varying,
            payment_status character varying(32) not null default 'open'::character varying,
            currency character varying(16),
            amount integer,
            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_checkout_pkey primary key (id),
            constraint paysub_checkout_nr unique (nr),
            constraint paysub_checkout_psp_id unique (psp, psp_checkout_id),
            constraint fk_paysub_checkout_rsc_id foreign key (rsc_id)
                references rsc (id)
                on update cascade
                on delete set null
        )", Context),
    [] = z_db:q("CREATE INDEX fki_paysub_checkout_rsc_id ON paysub_checkout (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_created_key ON paysub_checkout (created)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_modified_key ON paysub_checkout (modified)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_psp_psp_customer_id_key ON paysub_checkout (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_survey_id_key ON paysub_checkout (survey_id, survey_answer_id)", Context),

    [] = z_db:q("
        create table paysub_product (
            id serial not null,
            is_active boolean not null default true,
            user_group_id int,
            name character varying(128) not null,
            psp character varying(32) not null,
            psp_product_id character varying(128) not null,
            psp_default_price_id character varying(128),
            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_product_pkey primary key (id),
            constraint paysub_product_psp_product_id unique (psp, psp_product_id)
        )", Context),

    [] = z_db:q("CREATE INDEX paysub_product_name_key ON paysub_product (name)", Context),

    [] = z_db:q("
        create table paysub_price (
            id serial not null,
            is_active boolean not null default true,
            name character varying(128) not null,
            psp character varying(32) not null,
            psp_price_id character varying(128) not null,
            psp_product_id character varying(128) not null,
            props_json jsonb,
            currency character varying(16) not null default 'EUR'::character varying,
            amount integer not null default 0,
            is_recurring boolean not null default false,
            recurring_period character varying(16),
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_price_pkey primary key (id),
            constraint paysub_price_psp_price_id unique (psp, psp_price_id)
        )", Context),

    [] = z_db:q("CREATE INDEX paysub_price_name_key ON paysub_price (name)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_psp_product_key ON paysub_price (psp, psp_product_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_created_key ON paysub_price (created)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_modified_key ON paysub_price (modified)", Context),

    [] = z_db:q("
        create table paysub_subscription (
            id serial not null,
            rsc_id integer,
            psp character varying(32) not null,
            psp_subscription_id character varying(128) not null,
            psp_customer_id character varying(128) not null,
            is_provisioned boolean not null default false,
            status character varying(32) not null default ''::character varying,

            period_start timestamp with time zone,
            period_end timestamp with time zone,
            cancel_at timestamp with time zone,
            canceled_at timestamp with time zone,
            started_at timestamp with time zone,
            ended_at timestamp with time zone,
            trial_start timestamp with time zone,
            trial_end timestamp with time zone,

            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_pkey primary key (id),
            constraint paysub_psp_subscription_id unique (psp, psp_subscription_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_psp_customer_key ON paysub_subscription (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_status_key ON paysub_subscription (status)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_started_at_key ON paysub_subscription (started_at)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_ended_at_key ON paysub_subscription (ended_at)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_period_start_key ON paysub_subscription (period_start)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_period_end_at_key ON paysub_subscription (period_end)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_modified_key ON paysub_subscription (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_created_key ON paysub_subscription (created)", Context),

    [] = z_db:q("
        create table paysub_subscription_item (
            subscription_id int not null,
            psp character varying(32) not null,
            psp_price_id character varying(128) not null,
            created timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_item_pkey primary key (subscription_id, psp, psp_price_id),
            constraint fk_paysub_subscription_item_subscription_id foreign key (subscription_id)
                references paysub_subscription (id)
                on update cascade
                on delete cascade
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS fki_paysub_subscription_item_subscription_id ON paysub_subscription_item (subscription_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_item_psp_psp_price_id_key ON paysub_subscription_item (psp, psp_price_id)", Context),

    [] = z_db:q("
        create table paysub_customer (
            id serial not null,
            rsc_id integer,
            psp character varying(32) not null,
            psp_customer_id character varying(128) not null,
            name character varying(255),
            email character varying(255),
            pref_language character varying(16),
            address_country character varying(16),
            address_street_1 character varying(255),
            address_street_2 character varying(255),
            address_city character varying(255),
            address_state character varying(255),
            address_postcode character varying(255),
            phone character varying(255),

            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_customer_pkey primary key (id),
            constraint paysub_customer_psp_customer_id unique (psp, psp_customer_id),
            constraint fk_paysub_customer_rsc_id foreign key (rsc_id)
                references rsc (id)
                on update cascade
                on delete set null
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS fki_paysub_customer_rsc_id ON paysub_customer (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_customer_modified_key ON paysub_customer (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_customer_created_key ON paysub_customer (created)", Context),

    % payment_status is one of: draft, open, paid, uncollectible, or void
    [] = z_db:q("
        create table paysub_invoice (
            id serial not null,
            psp character varying(32) not null,
            psp_invoice_id character varying(128) not null,
            psp_customer_id character varying(128) not null,
            currency character varying(16) not null default 'EUR'::character varying,
            amount_due integer not null,
            amount_paid integer not null,
            amount_remaining integer not null,
            total integer not null,

            collection_method character varying(32) not null default 'charge_automatically'::character varying,
            is_payment_attempted boolean not null default false,
            payment_method character varying(128),
            payment_status character varying(32) not null default ''::character varying,
            period_start timestamp with time zone,
            period_end timestamp with time zone,

            name character varying(255),
            email character varying(255),
            address_country character varying(16),
            address_street_1 character varying(255),
            address_street_2 character varying(255),
            address_city character varying(255),
            address_state character varying(255),
            address_postcode character varying(255),

            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_invoice_pkey primary key (id),
            constraint paysub_invoice_psp_invoice_id unique (psp, psp_invoice_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_psp_customer_key ON paysub_invoice (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_modified_key ON paysub_invoice (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_created_key ON paysub_invoice (created)", Context),

    % status is one of: requires_payment_method, requires_confirmation, requires_action, processing,
    % requires_capture, canceled, or succeeded
    [] = z_db:q("
        create table paysub_payment (
            id serial not null,
            psp character varying(32) not null,
            psp_payment_id character varying(128) not null,
            psp_customer_id character varying(128),
            psp_invoice_id character varying(128),
            currency character varying(16) not null default 'EUR'::character varying,
            amount integer not null,
            amount_received integer not null,

            name character varying(255),
            email character varying(255),
            phone character varying(255),

            status character varying(32) not null default ''::character varying,
            description character varying (300),

            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_payment_pkey primary key (id),
            constraint paysub_payment_psp_payment_intent_id unique (psp, psp_payment_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_psp_customer_key ON paysub_payment (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_modified_key ON paysub_payment (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_created_key ON paysub_payment (created)", Context),

    % [] = z_db:q("
    %     create table paysub_log (
    %         id bigserial not null,
    %         rsc_id integer,
    %         event character varying(128) not null,
    %         psp character varying(32) not null,
    %         psp_customer_id character varying(128),
    %         psp_subscription_id character varying(128),
    %         props_json jsonb,
    %         created timestamp with time zone NOT NULL DEFAULT now(),

    %         constraint paysub_log_pkey primary key (id)
    %     )", Context),

    % [] = z_db:q("CREATE INDEX paysub_log_created_key ON paysub_log (created)", Context),
    % [] = z_db:q("CREATE INDEX paysub_log_rsc_id_key ON paysub_log (rsc_id)", Context),
    % [] = z_db:q("CREATE INDEX paysub_log_psp_psp_customer_id_key ON paysub_log (psp, psp_customer_id)", Context),
    % [] = z_db:q("CREATE INDEX paysub_log_psp_psp_subscription_id_key ON paysub_log (psp, psp_subscription_id)", Context),
    ok.

drop_tables(Context) ->
    z_db:q("drop table if exists paysub_log", Context),
    z_db:q("drop table if exists paysub_invoice_item", Context),
    z_db:q("drop table if exists paysub_invoice", Context),
    z_db:q("drop table if exists paysub_customer", Context),
    z_db:q("drop table if exists paysub_subscription_item", Context),
    z_db:q("drop table if exists paysub_subscription", Context),
    z_db:q("drop table if exists paysub_price", Context),
    z_db:q("drop table if exists paysub_product", Context),
    z_db:q("drop table if exists paysub_checkout", Context),
    z_db:q("drop table if exists paysub_payment", Context),
    ok.


