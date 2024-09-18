-module(m_paysub_search).

-export([
    m_get/3,

    search_query_term/2,
    search_query/4,

    list_countries/1
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([ <<"list">>, <<"countries">> | Rest ], _Msg, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            {ok, {list_countries(Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get([ <<"list">>, <<"cities">>, Country | Rest ], _Msg, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            {ok, {list_cities(Country, Context), Rest}};
        false ->
            {error, eacces}
    end;
m_get(_Path, _Msg, _Context) ->
    {error, notyet}.



%% @doc Extra query terms. Query for subscribers.
-spec search_query_term(QueryTerm, Context) -> SqlTerm when
    QueryTerm :: #search_query_term{},
    Context :: z:context(),
    SqlTerm :: #search_sql_term{}.
search_query_term(#search_query_term{ term = <<"is_subscriber">> } = T, Context) ->
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        text => <<"Use of deprecated search term 'is_subscriber', use 'is_paysub_subscriber' instead">>
    }),
    search_query_term(T#search_query_term{ term = <<"is_paysub_subscriber">> }, Context);
search_query_term(#search_query_term{ term = <<"is_paysub_subscriber">>, arg = Arg }, Context) ->
    PredId = m_rsc:rid(hasmaincontact, Context),
    Select = if
        PredId =:= undefined ->
            % All subscribers
            [
                <<" select sub.rsc_id
                    from paysub_subscription sub
                    where sub.status = any(">>, '$1', <<")
                      and sub.rsc_id is not null
                ">>
            ];
        is_integer(PredId) ->
            % All subscribers and their main contacts
            [
                <<" select sub.rsc_id
                    from paysub_subscription sub
                    where sub.status = any(">>, '$1', <<")
                      and sub.rsc_id is not null
                    union
                    select mce.object_id
                    from paysub_subscription sub
                        join edge mce
                        on mce.subject_id = sub.rsc_id
                        and mce.predicate_id = ", (integer_to_binary(PredId))/binary, "
                    where sub.status = any(">>, '$1', <<")
                      and sub.rsc_id is not null
                ">>
            ]
    end,
    States = m_paysub:pending_states(Context) ++ m_paysub:active_states(),
    case z_convert:to_bool(Arg) of
        true ->
            #search_sql_term{
                where = lists:flatten([
                    <<"rsc.id in (">>, Select, <<")">>
                ]),
                args = [
                    States
                ]
            };
        false ->
            #search_sql_term{
                where = lists:flatten([
                    <<"rsc.id not in (">>, Select, <<")">>
                ]),
                args = [
                    States
                ]
            }
    end;
search_query_term(#search_query_term{ term = <<"paysub_subscription_status">>, arg = Arg }, Context) ->
    case z_string:to_lower(z_convert:to_binary(Arg)) of
        <<"all_access">> ->
            % All subscribers with any active or pending subscription
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [ m_paysub:access_states(Context) ]
            };
        <<"all_active">> ->
            % All subscribers with any active subscription
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [ m_paysub:active_states() ]
            };
        <<"all_pending">> ->
            % All subscribers without an active subscription but with a pending subscription
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                    )
                    and rsc.id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$2', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [
                    m_paysub:pending_states(Context),
                    m_paysub:active_states()
                ]
            };
        <<"all_inactive">> ->
            % All subscribers without an active subscription, having no access
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                    )
                    and rsc.id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$2', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [
                    m_paysub:inactive_states(Context),
                    m_paysub:access_states(Context)
                ]
            };
        <<"pastdue_noaccess">> ->
            % All subscribers past-due without an active other subscription
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = 'past_due'
                          and paysub.rsc_id is not null
                    )
                    and rsc.id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [
                    m_paysub:access_states(Context) -- [ <<"past_due">> ]
                ]
            };
        <<"incomplete_noaccess">> ->
            % All subscribers with status incomplete without an other access subscription
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status in ('incomplete', 'incomplete_expired')
                          and paysub.rsc_id is not null
                    )
                    and rsc.id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(">>, '$1', <<")
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ],
                args = [
                    m_paysub:access_states(Context) -- [ <<"incomplete">>, <<"incomplete_expired">> ]
                ]
            };
        <<"incomplete_any">> ->
            % All subscribers with an incomplete subscription
            #search_sql_term{
                where = [
                    <<"select paysub.rsc_id
                       from paysub_subscription paysub
                       where paysub.status in ('incomplete', 'incomplete_expired')
                         and paysub.rsc_id is not null
                    ">>
                ]
            };
        <<"canceled_now">> ->
            % All subscribers that our in their cancelation period
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = 'canceled'
                          and paysub.rsc_id is not null
                          and paysub.cancel_at >= now()
                    )
                    and rsc.id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status in ('trialing', 'active', 'past_due')
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )">>
                ]
            };
        <<"canceled_m1">> ->
            % All subscribers that canceled in the last month
            Date = z_datetime:prev_month(calendar:universal_time()),
            search_sql_canceled_at(Date, Context);
        <<"canceled_y1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_year(calendar:universal_time()),
            search_sql_canceled_at(Date, Context);
        <<"new_w1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_week(calendar:universal_time()),
            search_sql_new_after(Date, Context);
        <<"new_m1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_month(calendar:universal_time()),
            search_sql_new_after(Date, Context);
        <<"new_y1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_year(calendar:universal_time()),
            search_sql_new_after(Date, Context);
        <<"now_", Status/binary>> ->
            % All subscribers with a certain status at this moment
            % where the subscription has not ended yet.
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = ">>, '$1', <<"
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                     )">>
                ],
                args = [ Status ]
            };
        Status ->
            % All subscribers with a certain status at any moment
            #search_sql_term{
                where = [
                    <<"rsc.id in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = ">>, '$1', <<"
                          and paysub.rsc_id is not null
                     )">>
                ],
                args = [ Status ]
            }
    end;
search_query_term(#search_query_term{}, _Context) ->
    undefined.

%% @doc Perform queries, helpers for the observer_search_query in mod_paysub.erl
search_query(invoices, #{ <<"rsc_id">> := undefined }, {Offset, Limit}, Context) ->
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
        total = m_paysub:count_invoices(Context),
        is_total_estimated = false
    };
search_query(invoices, #{ <<"rsc_id">> := UserId }, {Offset, Limit}, Context) ->
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
                total = m_paysub:count_user_invoices(RscId, Context),
                is_total_estimated = false
            }
    end;
search_query(subscriptions, Filters, {Offset, Limit}, Context) ->
    {Ws, Args} = subscription_terms(maps:to_list(Filters), [], [], Context),
    From = lists:flatten(["
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
        ",
        case is_join_rsc(Filters) of
            true ->
                " join rsc r on r.id = cust.rsc_id ";
            false ->
                ""
        end,
        if
            Ws =:= [] -> "";
            true -> [ "where ", lists:join(" and ", Ws) ]
        end
    ]),
    N = length(Args),
    Query = lists:flatten(["
        select sub.*,
               cust.id as customer_id,
               cust.psp_customer_id as psp_customer_id,
               cust.rsc_id as user_id,
               cust.name as name,
               cust.email as email,
               cust.address_country as country,
               product.name as product_name,
               price.name as price_name,
               price.currency as price_currency,
               price.amount as price_amount
        ", From, "
        order by sub.started_at desc, sub.id desc
        offset $", integer_to_list(N+1), "
        limit $", integer_to_list(N+2)
    ]),
    Count = lists:flatten(["
        select count(*) ", From
    ]),
    {ok, Subs} = z_db:qmap_props(Query, Args ++ [ Offset-1, Limit ], Context),
    Total = z_db:q1(Count, Args, Context),
    Subs1 = m_paysub:subscription_add_prices(Subs, Context),
    #search_result{
        result = Subs1,
        total = Total,
        is_total_estimated = false
    };
search_query(products, #{}, {Offset, Limit}, Context) ->
    {ok, Prods} = z_db:qmap_props("
        select *
        from paysub_product
        order by is_active desc, psp, name
        offset $1 limit $2
        ",
        [ Offset-1, Limit ],
        Context),
    Prods1 = m_paysub:products_add_prices(Prods, Context),
    #search_result{
        result = Prods1,
        total = m_paysub:count_products(Context),
        is_total_estimated = false
    };
search_query(payments, #{ <<"rsc_id">> := UserId }, {Offset, Limit}, Context) when UserId =/= undefined ->
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
                total = m_paysub:count_user_payments(RscId, Context),
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
        total = m_paysub:count_payments(Context),
        is_total_estimated = false
    }.

is_join_rsc(#{ <<"country">> := V }) when is_binary(V), V /= <<>> -> true;
is_join_rsc(#{ <<"city">> := V }) when is_binary(V), V /= <<>> -> true;
is_join_rsc(#{ <<"postcode">> := V }) when is_binary(V), V /= <<>> -> true;
is_join_rsc(_) -> false.

subscription_terms([], Ws, Args, _Context) ->
    {Ws, lists:reverse(Args)};
subscription_terms([ {_, undefined} | Ts ], Ws, Args, Context) ->
    subscription_terms(Ts, Ws, Args, Context);
subscription_terms([ {_, <<>>} | Ts ], Ws, Args, Context) ->
    subscription_terms(Ts, Ws, Args, Context);
subscription_terms([ {<<"rsc_id">>, Id} | Ts ], Ws, Args, Context) ->
    Args1 = [ m_rsc:rid(Id, Context) | Args ],
    Ws1 = [ "sub.rsc_id = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"country">>, V} | Ts ], Ws, Args, Context) when is_binary(V), V =/= <<>> ->
    Args1 = [ V | Args ],
    Ws1 = [ "r.pivot_country = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"country">>, _} | Ts ], Ws, Args, Context) ->
    subscription_terms(Ts, Ws, Args, Context);
subscription_terms([ {<<"city">>, V} | Ts ], Ws, Args, Context) when is_binary(V), V =/= <<>> ->
    Args1 = [ V | Args ],
    Ws1 = [ "r.pivot_city = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"city">>, _} | Ts ], Ws, Args, Context) ->
    subscription_terms(Ts, Ws, Args, Context);
subscription_terms([ {<<"postcode">>, V} | Ts ], Ws, Args, Context) when is_binary(V), V =/= <<>> ->
    Args1 = [ <<V/binary, "%">> | Args ],
    Ws1 = [ "r.pivot_postcode like $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"postcode">>, _} | Ts ], Ws, Args, Context) ->
    subscription_terms(Ts, Ws, Args, Context);
subscription_terms([ {<<"product_id">>, Id} | Ts ], Ws, Args, Context) ->
    Args1 = [ z_convert:to_integer(Id) | Args ],
    Ws1 = [ "product.id = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"user_group_id">>, Id} | Ts ], Ws, Args, Context) ->
    Args1 = [ m_rsc:rid(Id, Context) | Args ],
    Ws1 = [ "product.user_group_id = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"price_id">>, Id} | Ts ], Ws, Args, Context) ->
    Args1 = [ z_convert:to_integer(Id) | Args ],
    Ws1 = [ "price.id = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"psp">>, PSP} | Ts ], Ws, Args, Context) ->
    Args1 = [ PSP | Args ],
    Ws1 = [ "sub.psp = $" ++ integer_to_list(length(Args1)) | Ws ],
    subscription_terms(Ts, Ws1, Args1, Context);
subscription_terms([ {<<"status">>, Status} | Ts ], Ws, Args, Context) ->
    {Q,QArgs} = case z_string:to_lower(Status) of
        <<"all_access">> ->
            {"sub.status = any($1) and (sub.ended_at is null or sub.ended_at > now())",
             [ m_paysub:access_states(Context) ]};
        <<"all_active">> ->
            {"sub.status = any($1) and (sub.ended_at is null or sub.ended_at > now())",
             [ m_paysub:active_states() ]};
        <<"all_pending">> ->
            {"sub.status = any($1)",
             [ m_paysub:pending_states(Context) ]};
        <<"uncanceled">> ->
            {"sub.status = any($1)",
             [ m_paysub:all_states() -- [ <<"canceled">> ] ]};
        <<"all_inactive">> ->
            % All subscribers without an active subscription, having no access
            {" sub.status = any($1)
               and sub.rsc_id is not null
               and sub.rsc_id not in (
                    select paysub.rsc_id
                    from paysub_subscription paysub
                    where paysub.status = any($2)
                      and paysub.rsc_id is not null
                      and (paysub.ended_at is null or paysub.ended_at > now())
                )",
             [ m_paysub:inactive_states(Context), m_paysub:access_states(Context) ]};
        <<"pastdue_noaccess">> ->
            % All subscribers past-due without an active other subscription
            {"sub.status = 'past_due'
              and sub.rsc_id is not null
              and sub.rsc_id not in (
                        select paysub.rsc_id
                        from paysub_subscription paysub
                        where paysub.status = any(41)
                          and paysub.rsc_id is not null
                          and (ended_at is null or ended_at > now())
                    )",
             m_paysub:access_states(Context) -- [ <<"past_due">> ]};
        <<"incomplete_noaccess">> ->
            % All subscribers with status incomplete without an other access subscription
            {"sub.status in ('incomplete', 'incomplete_expired')
               and sub.rsc_id is not null
               and sub.rsc_id not in (
                    select paysub.rsc_id
                    from paysub_subscription paysub
                    where paysub.status = any($1)
                      and paysub.rsc_id is not null
                      and (ended_at is null or ended_at > now())
                )",
                [ m_paysub:access_states(Context) -- [ <<"incomplete">>, <<"incomplete_expired">> ] ]};
        <<"incomplete_any">> ->
            % All subscribers with an incomplete subscription
            {"sub.status in ('incomplete', 'incomplete_expired')", []};
        <<"canceled_now">> ->
            % All subscribers that our in their cancelation period
            {"sub.status = 'canceled' and sub.cancel_at >= now()", []};
        <<"canceled_m1">> ->
            % All subscribers that canceled in the last month
            Date = z_datetime:prev_month(calendar:universal_time()),
            {"sub.status = 'canceled' and sub.canceled_at >= $1", [ Date ]};
        <<"canceled_y1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_year(calendar:universal_time()),
            {"sub.status = 'canceled' and sub.canceled_at >= $1", [ Date ]};
        <<"new_w1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_week(calendar:universal_time()),
            {"sub.status = any($2)
              and (sub.ended_at is null or sub.ended_at > now())
              and (sub.started_at >= $1)",
              [ Date, m_paysub:access_states(Context) ]};
        <<"new_m1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_month(calendar:universal_time()),
            {"sub.status = any($2)
              and (sub.ended_at is null or sub.ended_at > now())
              and (sub.started_at >= $1)",
              [ Date, m_paysub:access_states(Context) ]};
        <<"new_y1">> ->
            % All subscribers that canceled in the last year
            Date = z_datetime:prev_year(calendar:universal_time()),
            {"sub.status = any($2)
              and (sub.ended_at is null or sub.ended_at > now())
              and (sub.started_at >= $1)",
              [ Date, m_paysub:access_states(Context) ]};
        <<"now_", StatusNow/binary>> ->
            % All subscribers with a certain status at this moment
            % where the subscription has not ended yet.
            {"sub.status = $1 and (sub.ended_at is null or sub.ended_at > now())", [ StatusNow ]};
        _ ->
            {"sub.status = $1", [ Status ]}
    end,
    Q1 = z_convert:to_binary(Q),
    {_, Q2} = lists:foldl(
        fun(_, {N, QAcc}) ->
            S = iolist_to_binary([ $$, integer_to_binary(N) ]),
            R = iolist_to_binary([ $$, integer_to_binary(N + length(Args)) ]),
            QAcc1 = binary:replace(QAcc, S, R, [ global ]),
            {N+1, QAcc1}
        end,
        {1, Q1},
        QArgs),
    Ws1 = Ws ++ [Q2],
    Args1 = lists:reverse(QArgs) ++ Args,
    subscription_terms(Ts, Ws1, Args1, Context).



search_sql_canceled_at(Date, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.id in (
                select paysub.rsc_id
                from paysub_subscription paysub
                where paysub.status = 'canceled'
                  and paysub.rsc_id is not null
                  and paysub.canceled_at >= ">>, '$1', <<"
            )
            and rsc.id not in (
                select paysub.rsc_id
                from paysub_subscription paysub
                where paysub.status = any(">>, '$2', <<")
                  and paysub.rsc_id is not null
                  and (ended_at is null or ended_at > now())
            )">>
        ],
        args = [
            Date,
            m_paysub:access_states(Context)
        ]
    }.


search_sql_new_after(Date, Context) ->
    #search_sql_term{
        where = [
            <<"rsc.id in (
                select paysub.rsc_id
                from paysub_subscription paysub
                where paysub.status = any(">>, '$2', <<")
                  and paysub.rsc_id is not null
                  and (ended_at is null or ended_at > now())
                  and (started_at >= ">>, '$1', <<")
            )">>
        ],
        args = [
            Date,
            m_paysub:access_states(Context)
        ]
    }.

list_countries(Context) ->
    Countries = z_db:q("
        select distinct pivot_country
        from rsc r
             join paysub_customer cust
             on r.id = cust.rsc_id
        where pivot_country is not null
          and pivot_country <> ''",
        Context),
    Ps = lists:map(
        fun({Code}) ->
            Country = m_l10n:country_name(Code, Context),
            {Country, Code}
        end,
        Countries),
    Ps1 = lists:sort(Ps),
    [ #{ <<"code">> => Code, <<"name">> => Country } || {Country, Code} <- Ps1 ].

list_cities(Country, Context) ->
    Cities = z_db:q("
        select distinct r.pivot_city
        from rsc r
             join paysub_customer cust
             on r.id = cust.rsc_id
        where r.pivot_city is not null
          and r.pivot_city <> ''
          and r.pivot_country = $1",
        [ Country ],
        Context),
    Ps = [ City || {City} <- Cities ],
    lists:sort(Ps).
