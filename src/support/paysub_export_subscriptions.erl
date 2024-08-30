%% @doc Export all subscriptions and their last invoice.

-module(paysub_export_subscriptions).

-export([
    filename/1,
    header/1,
    data/2,
    encode/3
    ]).

-include_lib("zotonic_core/include/zotonic.hrl").

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
    {ok, lists:map(fun(P) -> p(Data1, P, Context) end, cols())}.

fetch_data(Context) ->
    QArgs = z_context:get_qargs(Context),
    QArgs1 = [ {Arg, Value} || {<<"q", Arg/binary>>, Value} <- QArgs ],
    Filters = maps:from_list(QArgs1),
    #search_result{
        result = Rows
    } = m_paysub:search_query(subscriptions, Filters, {1, 100_000}, Context),
    {ok, Rows}.

add_invoice(Sub, Context) ->
    #{
        <<"rsc_id">> := UserId
    } = Sub,
    Inv = fetch_last_invoice_paid(UserId, Context),
    Sub1 = maps:merge(Inv, Sub),
    Sub1#{
        <<"subscription_year">> => fetch_first_subscription_year(UserId, Context)
    }.


fetch_last_invoice_paid(UserId, Context) ->
    case z_db:qmap_props_row("
        select inv.created as invoice_date,
               inv.payment_status as invoice_status,
               inv.currency as invoice_currency,
               inv.amount_due as invoice_amount_due,
               inv.amount_paid as invoice_amount_paid,
               inv.amount_remaining as invoice_amount_remaining,
               inv.props_json
        from paysub_invoice inv
            join paysub_customer cust
                on  inv.psp = cust.psp
                and inv.psp_customer_id = cust.psp_customer_id
        where inv.payment_status in ('paid', 'no_payment_required')
          and cust.rsc_id = $1
        order by inv.created desc
        limit 1
        ",
        [ UserId ],
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

fetch_first_subscription_year(UserId, Context) ->
    case z_db:q1("
        select min(sub.started_at)
        from paysub_subscription sub
        where sub.rsc_id = $1
        ",
        [ UserId ],
        Context)
    of
        {{Y, _, _}, _} -> Y;
        undefined -> undefined
    end.

p(#{ <<"rsc_id">> := Id }, <<"country">>, Context) ->
    C = case m_rsc:p(Id, <<"billing_country">>, Context) of
        undefined -> m_rsc:p(Id, <<"address_country">>, Context);
        V -> V
    end,
    m_l10n:country_name(C, Context);
p(#{ <<"rsc_id">> := Id }, <<"phone">>, Context) ->
    case m_rsc:p(Id, <<"phone">>, Context) of
        undefined -> m_rsc:p(Id, <<"phone_mobile">>, Context);
        V -> V
    end;
p(#{ <<"rsc_id">> := Id }, <<"city">>, Context) ->
    case m_rsc:p(Id, <<"billing_country">>, Context) of
        undefined -> m_rsc:p(Id, <<"address_city">>, Context);
        _ -> m_rsc:p(Id, <<"billing_city">>, Context)
    end;
p(#{ <<"rsc_id">> := Id }, <<"postcode">>, Context) ->
    case m_rsc:p(Id, <<"billing_country">>, Context) of
        undefined -> m_rsc:p(Id, <<"address_postcode">>, Context);
        _ -> m_rsc:p(Id, <<"billing_postcode">>, Context)
    end;
p(#{ <<"rsc_id">> := Id }, <<"street">>, Context) ->
    C = case m_rsc:p(Id, <<"billing_country">>, Context) of
        undefined -> m_rsc:p(Id, <<"address_street_1">>, Context);
        _ -> m_rsc:p(Id, <<"billing_street_1">>, Context)
    end,
    m_l10n:country_name(C, Context);
p(#{ <<"rsc_id">> := Id }, <<"email">>, Context) ->
    case m_rsc:p(Id, <<"billing_email">>, Context) of
        undefined -> m_rsc:p(Id, <<"email">>, Context);
        V -> V
    end;
p(#{ <<"rsc_id">> := Id }, <<"website">>, Context) ->
    m_rsc:p(Id, <<"website">>, Context);
p(#{ <<"rsc_id">> := Id }, <<"name_first">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    m_rsc:p(MId, <<"name_first">>, Context);
p(#{ <<"rsc_id">> := Id }, <<"name_surname">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    m_rsc:p(MId, <<"name_surname">>, Context);
p(#{ <<"rsc_id">> := Id }, <<"contact country">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    C = case m_rsc:p(MId, <<"billing_country">>, Context) of
        undefined -> m_rsc:p(MId, <<"address_country">>, Context);
        V -> V
    end,
    m_l10n:country_name(C, Context);
p(#{ <<"rsc_id">> := Id }, <<"contact phone">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    case m_rsc:p(MId, <<"phone">>, Context) of
        undefined -> m_rsc:p(MId, <<"phone_mobile">>, Context);
        V -> V
    end;
p(#{ <<"rsc_id">> := Id }, <<"contact email">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    case m_rsc:p(MId, <<"billing_email">>, Context) of
        undefined -> m_rsc:p(MId, <<"email">>, Context);
        V -> V
    end;
p(#{ <<"rsc_id">> := Id }, <<"pref_language">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    m_rsc:p(MId, <<"pref_language">>, Context);
p(#{ <<"price_amount">> := Amount, <<"price_currency">> := _Currency }, <<"price_amount">>, _Context) when is_integer(Amount) ->
    Amount / 100;
p(#{ <<"items">> := [ #{ <<"user_group_id">> := UserGroupId } ] }, <<"user_group">>, Context) ->
    m_rsc:p_no_acl(UserGroupId, <<"title">>, Context);
p(#{}, <<"user_group">>, _Context) ->
    <<"-">>;
p(Data, P, _Context) ->
    maps:get(P, Data, undefined).

cols() ->
    [
        <<"user_id">>,
        <<"name">>,
        <<"email">>,
        <<"phone">>,
        <<"street">>,
        <<"city">>,
        <<"postcode">>,
        <<"country">>,
        <<"name_first">>,
        <<"name_surname">>,
        <<"pref_language">>,
        <<"contact email">>,
        <<"contact phone">>,
        <<"contact country">>,
        <<"website">>,
        <<"psp">>,
        <<"psp_subscription_id">>,
        <<"status">>,
        <<"product_name">>,
        <<"user_group">>,
        <<"price_name">>,
        <<"price_currency">>,
        <<"price_amount">>,
        <<"subscription_year">>,
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
