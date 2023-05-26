%% @doc Export all subscriptions and their last invoice.

-module(paysub_export_subscriptions).

-export([
    filename/1,
    header/1,
    data/2,
    encode/3
    ]).

filename(_Context) ->
    {ok, <<"subscriptions">>}.

header(_Context) ->
    {ok, cols()}.

data(_State, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            fetch_data(Context);
        false ->
            {error, eacces}
    end.

encode(Data, _State, Context) ->
    Data1 = add_invoice(Data, Context),
    {ok, lists:map(fun(P) -> p(Data1, P) end, cols())}.

fetch_data(Context) ->
    z_db:qmap("
        select
            sub.psp as psp,
            sub.psp_subscription_id as psp_subscription_id,
            sub.status as status,
            sub.started_at as started_at,
            sub.ended_at as ended_at,
            sub.cancel_at as cancel_at,
            sub.canceled_at as canceled_at,
            sub.period_start as period_start,
            sub.period_end as period_end,
            cust.psp_customer_id as psp_customer_id,
            cust.rsc_id as user_id,
            cust.name as name,
            cust.email as email,
            cust.address_country as country,
            prod.name as product_name,
            price.name as price_name,
            price.currency as price_currency,
            price.amount as price_amount
        from paysub_subscription sub
            join paysub_subscription_item item
                on item.subscription_id = sub.id
            join paysub_price price
                on item.psp = price.psp
                and item.psp_price_id = price.psp_price_id
            join paysub_product prod
                on prod.psp = price.psp
                and prod.psp_product_id = price.psp_product_id
            left join paysub_customer cust
                on cust.psp = sub.psp
                and cust.psp_customer_id = sub.psp_customer_id
        order by sub.started_at desc
        ",
        Context).

add_invoice(Sub, Context) ->
    #{
        <<"psp">> := Psp,
        <<"psp_customer_id">> := PspCustomerId
    } = Sub,
    Inv = fetch_last_invoice(Psp, PspCustomerId, Context),
    maps:merge(Sub, Inv).

fetch_last_invoice(Psp, PspCustomerId, Context) ->
    case z_db:qmap_props_row("
        select created as invoice_date,
               payment_status as invoice_status,
               currency as invoice_currency,
               amount_due as invoice_amount_due,
               amount_paid as invoice_amount_paid,
               amount_remaining as invoice_amount_remaining,
               props_json
        from paysub_invoice
        where psp = $1
          and psp_customer_id = $2
        order by created desc
        limit 1
        ",
        [ Psp, PspCustomerId ],
        Context)
    of
        {ok, #{ <<"items">> := [Item] } = Inv} ->
            Inv#{
                <<"invoice_description">> => maps:get(<<"description">>, Item, <<>>)
            };
        {ok, Inv} ->
            Inv;
        {error, enoent} ->
            #{}
    end.


p(Data, P) ->
    maps:get(P, Data, undefined).

cols() ->
    [
        <<"user_id">>,
        <<"name">>,
        <<"email">>,
        <<"psp">>,
        <<"psp_subscription_id">>,
        <<"status">>,
        <<"product_name">>,
        <<"price_name">>,
        <<"price_currency">>,
        <<"price_amount">>,
        <<"started_at">>,
        <<"ended_at">>,
        <<"cancel_at">>,
        <<"canceled_at">>,
        <<"period_start">>,
        <<"period_end">>,
        <<"invoice_date">>,
        <<"invoice_status">>,
        <<"invoice_amount_due">>,
        <<"invoice_amount_paid">>,
        <<"invoice_amount_remaining">>,
        <<"invoice_description">>
    ].


