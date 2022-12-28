%% @copyright 2022 Marc Worrell
%% @doc Model for paid subscriptions
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


%% Subscription status
%% ===================
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

-module(m_paysub).

-export([
    m_get/3,

    is_allowed_paysub/1,

    search_query/4,
    count_invoices/1,

    user_groups/2,
    user_subscriptions/2,
    user_customers/2,
    user_invoices/2,

    checkout_status/2,
    checkout_create/5,
    checkout_update/4,
    get_customer/3,

    list_products/2,
    list_prices/2,
    list_customers/2,

    sync_products/3,
    sync_product/3,
    delete_product/3,

    sync_prices/3,
    sync_price/3,
    delete_price/3,

    sync_subscription/4,
    delete_subscription/3,

    update_customer_rsc_id/4,
    sync_customer/3,
    sync_customers/3,
    delete_customer/3,

    sync_invoice/3,
    sync_invoices/3,
    delete_invoice/3,

    install/1,
    drop_tables/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

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
            case is_allowed_paysub(Context) of
                true ->
                    {ok, {count_user_invoices(RId, Context), Rest}};
                false when RId =:= UserId ->
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
            Res = case is_allowed_paysub(Context) of
                true ->
                    user_invoices(RId, Context);
                false when RId =:= UserId ->
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
    end.

is_allowed_paysub(Context) ->
    z_acl:is_admin(Context) orelse z_acl:is_allowed(use, mod_paysub, Context).

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
                    join paysub_subscription_item item
                    and prod.psp_product_id = item.psp_product_id
                    join paysub_subscription sub
                    on sub.id = item.subscription_id
                    and sub.psp = prod.psp
                where sub.status in ('incomplete', 'trialing', 'active', 'past_due')
                  and sub.rsc_id = $1
                  and prod.user_group_id is not null
                ", [ UserId ], Context),
            [ Id || {Id} <- Ids ]
    end.


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
                order by period_end desc, created desc",
                [ UserId ],
                Context),
            Subs1 = lists:map(
                fun(#{ <<"id">> := Id, <<"psp">> := PSP } = Sub) ->
                    {ok, Prices} = z_db:qmap_props("
                        select *
                        from paysub_price price
                            left join paysub_subscription_item item
                            on price.psp_price_id = item.psp_price_id
                            and price.psp = $1
                            left join paysub_product prod
                            on prod.psp = price.psp
                            and prod.psp_product_id = price.psp_product_id
                        where item.subscription_id = $2
                        ",
                        [ PSP, Id ],
                        Context),
                    Sub#{
                        <<"items">> => Prices
                    }
                end,
                Subs),
            {ok, Subs1}
    end.

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
                where cus.rsc_id = $1
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



-spec checkout_status(CheckoutNr, Context) -> {ok, map()} | {error, term()} when
    CheckoutNr :: binary(),
    Context :: z:context().
checkout_status(CheckoutNr, Context) ->
    case z_db:q_row("
        select status, payment_status
        from paysub_checkout
        where nr = $1",
        [ CheckoutNr ], Context)
    of
        { <<"open">>, _ } ->
            {ok, #{
                is_failed => false,
                is_paid => false
            }};
        { <<"complete">>, <<"paid">> } ->
            {ok, #{
                is_failed => false,
                is_paid => true
            }};
        { <<"complete">>, <<"no_payment_required">> } ->
            {ok, #{
                is_failed => false,
                is_paid => true
            }};
        { <<"complete">>, _ } ->
            {ok, #{
                is_failed => false,
                is_paid => false
            }};
        { <<"expired">>, _ } ->
            {ok, #{
                is_failed => true,
                is_paid => false
            }};
        undefined ->
            {error, enoent}
    end.


-spec checkout_create(PSP, UserId, Mode, Args, Context) -> Result when
    PSP :: atom() | binary(),
    UserId :: undefined | m_rsc:resource_id(),
    Mode :: payment | subscription,
    Args :: map(),
    Context :: z:context(),
    Result :: {ok, CheckoutNr} | {error, term()},
    CheckoutNr :: binary().
checkout_create(PSP, UserId, Mode, Args, Context) ->
    Nr = z_ids:id(32),
    case z_db:insert(
        paysub_checkout,
        #{
            rsc_id => UserId,
            psp => PSP,
            mode => Mode,
            nr => Nr,
            status => <<"open">>,           % open, complete, or expired.
            payment_status => <<"unpaid">>, % unpaid, paid, no_payment_required
            args => Args
        },
        Context)
    of
        {ok, _} ->
            {ok, Nr};
        {error, _} = Error ->
            Error
    end.

-spec checkout_update(PSP, CheckoutNr, Update, Context) -> Result when
    PSP :: atom() | binary(),
    CheckoutNr :: binary(),
    Update :: map(),
    Context :: z:context(),
    Result :: ok | {error, enoent}.
checkout_update(PSP, CheckoutNr, Update, Context) ->
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
            % TODO:
            % - Notification with new status
            z_db:update(
                paysub_checkout,
                CheckoutId,
                Update#{ modified => calendar:universal_time() },
                Context)
    end.


-spec get_customer(PSP, UserIdOrCustId, Context) -> Result when
    PSP :: atom() | binary(),
    UserIdOrCustId :: undefined | m_rsc:resource_id() | binary(),
    Context :: z:context(),
    Result :: {ok, map()} | {error, term()}.
get_customer(_PSP, undefined, _Context) ->
    {error, enoent};
get_customer(PSP, UserId, Context) when is_integer(UserId) ->
    z_db:qmap_props_row("
        select *
        from paysub_customer
        where rsc_id = $1
          and psp = $2
        ", [ UserId, PSP ], Context);
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
    end.


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


update_customer_rsc_id(PSP, CustId, RscId, Context) ->
    case z_db:q("
        update paysub_customer
        set rsc_id = $3
        where psp = $1
          and psp_customer_id = $2",
        [ PSP, CustId, RscId ],
        Context)
    of
        1 ->
            ok;
        0 ->
            {error, enoent}
    end.

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
    case z_db:q1("
        select id
        from paysub_customer
        where psp = $1
          and psp_customer_id = $2",
        [ PSP, CustId ],
        Context)
    of
        undefined ->
            Cust1 = Cust#{ psp => PSP },
            {ok, _} = z_db:insert(paysub_customer, Cust1, Context);
        Id ->
            {ok, _} = z_db:update(paysub_customer, Id, Cust, Context)
    end.

-spec delete_customer(PSP, Id, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    Id :: binary(),
    Context :: z:context().
delete_customer(PSP, Id, Context) ->
    case z_db:q("
        delete from paysub_customer
        where psp = $1
          and psp_customer_id = $1",
        [ PSP, Id ],
        Context)
    of
        1 -> ok;
        0 -> {error, enoent}
    end.

-spec sync_subscription(PSP, Sub, PriceIds, Context) -> ok when
    PSP :: atom() | binary(),
    Sub :: map(),
    PriceIds :: list( binary() ),
    Context :: z:context().
sync_subscription(PSP, #{ psp_subscription_id := PspSubId } = Sub, PriceIds, Context) ->
    {ok, SubId} = case z_db:q1("
        select id
        from paysub_subscription
        where psp = $1
          and psp_subscription_id = $2",
        [ PSP, PspSubId ],
        Context)
    of
        undefined ->
            Sub1 = Sub#{ psp => PSP, modified => calendar:universal_time() },
            {ok, _} = z_db:insert(paysub_subscription, Sub1, Context);
        Id ->
            Sub1 = Sub#{ modified => calendar:universal_time() },
            {ok, _} = z_db:update(paysub_subscription, Id, Sub1, Context)
    end,
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
                  and psp_price_id = $2",
                [ SubId, PriceId ],
                Context)
        end,
        Del),
    lists:foreach(
        fun(PriceId) ->
            z_db:q("
                insert into paysub_subscription_item
                    (subscription_id, psp_price_id)
                values
                    ($1, $2)",
                [ SubId, PriceId ],
                Context)
        end,
        New).

-spec delete_subscription(PSP, Id, Context) -> ok | {error, enoent} when
    PSP :: atom() | binary(),
    Id :: binary(),
    Context :: z:context().
delete_subscription(PSP, Id, Context) ->
    case z_db:q("
        delete from paysub_subscription
        where psp = $1
          and psp_subscription_id = $1",
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
sync_invoice(PSP, #{ psp_invoice_id := InvId } = Prod, Context) ->
    case z_db:q1("
        select id
        from paysub_invoice
        where psp = $1
          and psp_invoice_id = $2",
        [ PSP, InvId ],
        Context)
    of
        undefined ->
            Prod1 = Prod#{ psp => PSP },
            {ok, _} = z_db:insert(paysub_invoice, Prod1, Context);
        Id ->
            {ok, _} = z_db:update(paysub_invoice, Id, Prod, Context)
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



-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(paysub_checkout, Context) of
        false ->
            ok = install_tables(Context),
            z_db:flush(Context),
            ok;
        true ->
            ok
    end.

install_tables(Context) ->
    [] = z_db:q("
        create table paysub_checkout (
            id serial not null,
            rsc_id integer,
            nr character varying(32) not null,
            psp character varying(32) not null,
            psp_checkout_id character varying(128),
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
    [] = z_db:q("CREATE INDEX paysub_created_key ON paysub_price (created)", Context),

    [] = z_db:q("
        create table paysub_subscription (
            id serial not null,
            rsc_id integer,
            psp character varying(32) not null,
            psp_subscription_id character varying(128) not null,
            psp_customer_id character varying(128) not null,
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

    [] = z_db:q("CREATE INDEX paysub_psp_customer_key ON paysub_subscription (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_status_key ON paysub_subscription (status)", Context),
    [] = z_db:q("CREATE INDEX paysub_started_at_key ON paysub_subscription (started_at)", Context),
    [] = z_db:q("CREATE INDEX paysub_ended_at_key ON paysub_subscription (ended_at)", Context),
    [] = z_db:q("CREATE INDEX paysub_modified_key ON paysub_subscription (modified)", Context),

    [] = z_db:q("
        create table paysub_subscription_item (
            subscription_id int not null,
            psp_price_id character varying(128) not null,
            created timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_item_pkey primary key (subscription_id, psp_price_id),
            constraint fk_paysub_subscription_item_subscription_id foreign key (subscription_id)
                references paysub_subscription (id)
                on update cascade
                on delete cascade
        )", Context),
    [] = z_db:q("CREATE INDEX fki_paysub_subscription_item_subscription_id ON paysub_subscription_item (subscription_id)", Context),

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

            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_customer_pkey primary key (id),
            constraint paysub_customer_psp_customer_id unique (psp, psp_customer_id),
            constraint fk_paysub_customer_rsc_id foreign key (rsc_id)
                references rsc (id)
                on update cascade
                on delete set null
        )", Context),

    [] = z_db:q("CREATE INDEX fki_paysub_customer_rsc_id ON paysub_customer (rsc_id)", Context),

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
    [] = z_db:q("CREATE INDEX paysub_invoice_psp_customer_key ON paysub_invoice (psp, psp_customer_id)", Context),

    [] = z_db:q("
        create table paysub_log (
            id bigserial not null,
            rsc_id integer,
            event character varying(128) not null,
            psp character varying(32) not null,
            psp_customer_id character varying(128),
            psp_subscription_id character varying(128),
            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_log_pkey primary key (id)
        )", Context),

    [] = z_db:q("CREATE INDEX paysub_log_created_key ON paysub_log (created)", Context),
    [] = z_db:q("CREATE INDEX paysub_log_rsc_id_key ON paysub_log (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_log_psp_psp_customer_id_key ON paysub_log (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_log_psp_psp_subscription_id_key ON paysub_log (psp, psp_subscription_id)", Context),
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
    ok.


