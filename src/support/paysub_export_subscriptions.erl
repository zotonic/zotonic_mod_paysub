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
p(#{ <<"rsc_id">> := Id }, <<"email">>, Context) ->
    case m_rsc:p(Id, <<"billing_email">>, Context) of
        undefined -> m_rsc:p(Id, <<"email">>, Context);
        V -> V
    end;
p(#{ <<"rsc_id">> := Id }, <<"contact name">>, Context) ->
    MId = case m_edge:objects(Id, hasmaincontact, Context) of
        [Main|_] -> Main;
        [] -> Id
    end,
    {Name, _} = z_template:render_to_iolist("_name.tpl", [ {id, MId} ], Context),
    iolist_to_binary(Name);
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
p(Data, P, _Context) ->
    maps:get(P, Data, undefined).

cols() ->
    [
        <<"user_id">>,
        <<"name">>,
        <<"email">>,
        <<"phone">>,
        <<"country">>,
        <<"contact name">>,
        <<"contact email">>,
        <<"contact phone">>,
        <<"contact country">>,
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
