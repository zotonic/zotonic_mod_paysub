%% @copyright 2026 Marc Worrell
%% @doc Isolated PostgreSQL regressions. Set PAYMENT_TEST_PORT to opt in.
-module(paysub_coupon_db_tests).
-export([observe_checkout/2]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../include/paysub.hrl").

coupon_db_test_() ->
    case os:getenv("PAYMENT_TEST_PORT") of
        false -> [];
        Port -> {setup, fun() -> setup(list_to_integer(Port)) end, fun cleanup/1,
            fun(C) -> {timeout, 60, fun() -> exercise(z_context:set(owner, self(), C)) end} end}
    end.

setup(Port) ->
    application:ensure_all_started(epgsql),
    application:ensure_all_started(crypto),
    application:ensure_all_started(zotonic_notifier),
    Pool = spawn(fun() -> receive stop -> ok end end),
    register(paysub_coupon_test_pool, Pool),
    Args = [{dbhost, "127.0.0.1"}, {dbport, Port}, {dbuser, "payment_test"},
        {dbpassword, "unused"}, {dbdatabase, "postgres"}, {dbschema, "paysub_coupon_test"}],
    {ok, Worker} = z_db_pgsql:start_link(Args),
    C = z_context:set([{worker, Worker}, {dbargs, Args}],
        #context{site = paysub_coupon_test, db = {paysub_coupon_test_pool, z_db_pgsql}}),
    meck:new([z_db_pool, z_depcache, z_stats, z_notifier, m_identity], [passthrough, no_link]),
    meck:expect(z_db_pool, get_connection, fun(Ctx) -> {ok, z_context:get(worker, Ctx)} end),
    meck:expect(z_db_pool, return_connection, fun(_, _) -> ok end),
    meck:expect(z_db_pool, get_database_options, fun(_) -> Args end),
    meck:expect(z_depcache, get, fun(_, _) -> undefined end),
    meck:expect(z_depcache, set, fun(_, _, _, _, _) -> ok end),
    meck:expect(z_depcache, flush, fun(_, _) -> ok end),
    meck:expect(z_stats, record_event, fun(_, _, _) -> ok end),
    meck:expect(z_stats, record_duration, fun(_, _, _, _) -> ok end),
    meck:expect(z_notifier, first, fun(_, _) -> undefined end),
    meck:expect(z_notifier, notify_queue, fun(_) -> ok end),
    meck:expect(z_notifier, notify_queue_flush, fun(_) -> ok end),
    meck:expect(m_identity, get_user_info, fun(_, _) -> #{} end),
    z_db:q("create schema paysub_coupon_test", C),
    z_db:q("create table rsc (id serial primary key)", C),
    ok = paysub_schema:install(C),
    C.

cleanup(C) ->
    z_db:q("drop schema paysub_coupon_test cascade", C),
    gen_server:stop(z_context:get(worker, C)),
    whereis(paysub_coupon_test_pool) ! stop,
    meck:unload([z_db_pool, z_depcache, z_stats, z_notifier, m_identity]).

exercise(C) ->
    ok = zotonic_notifier:observe({paysub_coupon_test, paysub_checkout_done},
        {?MODULE, observe_checkout}, self()),
    upgrade(C),
    legacy_enrichment(C),
    expired_ordering(C),
    pid_observer(C),
    lists:foreach(fun(Discount) ->
        Nr = integer_to_binary(Discount),
        new_checkout(Nr, C),
        S = (paysub_coupon_tests:session(Discount))#{<<"client_reference_id">> => Nr,
            <<"id">> => <<"cs_", Nr/binary>>},
        % Complete but unpaid can notify once; async success must notify again.
        Unpaid = S#{<<"payment_status">> => <<"unpaid">>},
        ok = paysub_stripe:checkout_session_completed(Unpaid, C),
        notification(false),
        ok = paysub_stripe:checkout_session_completed(Unpaid, C),
        no_notification(),
        concurrent_completion(S, C),
        notification(true),
        no_notification(),
        {ok, Settled} = row(Nr, C),
        % Completion can arrive before the API creation response stores its id.
        ?assertEqual(maps:get(<<"id">>, S), maps:get(<<"psp_checkout_id">>, Settled)),
        lists:foreach(fun(Old) ->
            ok = paysub_stripe:checkout_session_completed(Old, C),
            ?assertEqual({ok, Settled}, row(Nr, C)),
            no_notification()
        end, [S, Unpaid#{<<"amount_total">> => 5000},
            Unpaid#{<<"status">> => <<"open">>}, Unpaid#{<<"status">> => <<"expired">>}]),
        Props = maps:get(<<"props_json">>, Settled),
        Details = maps:get(<<"discount_details">>, Props),
        ?assertEqual(Discount, maps:get(<<"amount_discount">>, maps:get(<<"total_details">>, Details))),
        ?assertEqual(maps:get(<<"discounts">>, S), maps:get(<<"discounts">>, Details))
    end, [2500, 5000]),
    ok = paysub_stripe:sync_invoice(paysub_coupon_tests:invoice(5000), C),
    {ok, Invoice} = z_db:qmap_row("select * from paysub_invoice where psp_invoice_id = 'in_coupon'", C),
    InvoiceProps = maps:get(<<"props_json">>, Invoice),
    ?assertEqual(5000, maps:get(<<"subtotal">>, maps:get(<<"discount_details">>, InvoiceProps))),
    [Item] = maps:get(<<"items">>, InvoiceProps),
    ?assertEqual([#{<<"amount">> => 5000}], maps:get(<<"discount_amounts">>, maps:get(<<"discount_details">>, Item))),
    new_checkout(<<"retry">>, C),
    S = (paysub_coupon_tests:session(5000))#{<<"client_reference_id">> => <<"retry">>},
    ?assertMatch({error, _}, paysub_stripe:checkout_session_completed(S, z_context:set(fail_notify, true, C))),
    {ok, Failed} = row(<<"retry">>, C),
    ?assertEqual(undefined, maps:get(<<"notified_state">>, Failed)),
    ?assertMatch({error, _}, paysub_stripe:checkout_session_completed(S, z_context:set(fail_notify, crash, C))),
    {ok, Failed} = row(<<"retry">>, C),
    ok = paysub_stripe:checkout_session_completed(S, C),
    notification(true),
    no_notification().

%% Exercise the real notifier, including returned errors (notify_sync discards them).
observe_checkout(N, Context) ->
    case z_context:get(fail_notify, Context) of
        true -> {error, test_observer_failure};
        crash -> error(test_observer_crash);
        _ -> z_context:get(owner, Context) ! N, ok
    end.

legacy_enrichment(C) ->
    S = (paysub_coupon_tests:session(5000))#{<<"client_reference_id">> => <<"old-free">>,
        <<"id">> => <<"cs_legacy_free">>},
    {ok, Before} = row(<<"old-free">>, C),
    ok = paysub_stripe:checkout_session_completed(S, C),
    {ok, After} = row(<<"old-free">>, C),
    ?assertEqual(maps:get(<<"amount">>, Before), maps:get(<<"amount">>, After)),
    ?assertEqual(maps:get(<<"notified_state">>, Before), maps:get(<<"notified_state">>, After)),
    ?assertEqual(<<"cs_legacy_free">>, maps:get(<<"psp_checkout_id">>, After)),
    Details = maps:get(<<"discount_details">>, maps:get(<<"props_json">>, After)),
    ?assertEqual(maps:get(<<"discounts">>, S), maps:get(<<"discounts">>, Details)),
    % Older/incomplete snapshots cannot erase recovered coupon evidence.
    ok = paysub_stripe:checkout_session_completed(maps:remove(<<"discounts">>, S), C),
    ?assertEqual({ok, After}, row(<<"old-free">>, C)),
    lists:foreach(fun(Bad) ->
        ?assertEqual({error, checkout_mismatch}, paysub_stripe:checkout_session_completed(Bad, C)),
        ?assertEqual({ok, After}, row(<<"old-free">>, C))
    end, [S#{<<"amount_total">> => 1}, S#{<<"currency">> => <<"usd">>},
          S#{<<"id">> => <<"different_session">>}]),
    no_notification().

expired_ordering(C) ->
    new_checkout(<<"expired">>, C),
    S = (paysub_coupon_tests:session(2500))#{<<"client_reference_id">> => <<"expired">>,
        <<"id">> => <<"cs_expired">>, <<"status">> => <<"expired">>, <<"payment_status">> => <<"unpaid">>},
    ok = paysub_stripe:checkout_session_sync(S, C),
    {ok, Expired} = row(<<"expired">>, C),
    ok = paysub_stripe:checkout_session_sync(S#{<<"status">> => <<"open">>}, C),
    ?assertEqual({ok, Expired}, row(<<"expired">>, C)),
    no_notification().

%% PID observers retain notify_sync's cast protocol, rather than becoming calls.
pid_observer(C) ->
    Parent = self(),
    Pid = spawn(fun() ->
        receive
            {'$gen_cast', {#paysub_checkout_done{}, _}} -> Parent ! pid_notified;
            {'$gen_call', From, _} -> gen_server:reply(From, {error, unexpected_call})
        end
    end),
    Event = {paysub_coupon_test, paysub_checkout_done},
    ok = zotonic_notifier:observe(Event, Pid, Pid),
    try
        new_checkout(<<"pid-observer">>, C),
        S = (paysub_coupon_tests:session(5000))#{<<"id">> => <<"cs_pid">>,
            <<"client_reference_id">> => <<"pid-observer">>},
        ok = paysub_stripe:checkout_session_completed(S, C),
        receive pid_notified -> ok after 1000 -> error(no_pid_notification) end,
        notification(true),
        no_notification()
    after
        zotonic_notifier:detach(Event, Pid),
        exit(Pid, shutdown)
    end.

new_checkout(Nr, C) ->
    {ok, _} = z_db:insert(paysub_checkout, #{nr => Nr, psp => stripe, mode => payment,
        amount => 5000, currency => <<"EUR">>, status => <<"open">>, payment_status => <<"unpaid">>}, C), ok.
row(Nr, C) -> z_db:qmap_row("select * from paysub_checkout where nr = $1", [Nr], C).
notification(IsPaid) ->
    receive #paysub_checkout_done{checkout_status = S} -> ?assertEqual(IsPaid, maps:get(<<"is_paid">>, S))
    after 1000 -> error(missing_notification) end.
no_notification() -> receive #paysub_checkout_done{} -> error(duplicate_notification) after 0 -> ok end.

concurrent_completion(S, C) ->
    Parent = self(),
    Pids = [spawn_monitor(fun() ->
        {ok, W} = z_db_pgsql:start_link(z_context:get(dbargs, C)),
        try Parent ! {done, paysub_stripe:checkout_session_completed(S, z_context:set(worker, W, C))}
        after gen_server:stop(W) end
    end) || _ <- lists:seq(1, 6)],
    [receive {done, ok} -> ok after 10000 -> error(completion_timeout) end || _ <- Pids],
    [receive {'DOWN', Ref, process, Pid, normal} -> ok after 10000 -> error(worker_failed) end
        || {Pid, Ref} <- Pids].

upgrade(C) ->
    Columns = z_db:column_names(paysub_checkout, C),
    new_checkout(<<"old-paid">>, C),
    new_checkout(<<"old-pending">>, C),
    new_checkout(<<"old-free">>, C),
    new_checkout(<<"old-unpaid-complete">>, C),
    z_db:q("update paysub_checkout set status = 'complete' where nr = 'old-unpaid-complete'", C),
    z_db:q("update paysub_checkout set status = 'complete', payment_status = 'no_payment_required', amount = 0 where nr = 'old-free'", C),
    z_db:q("update paysub_checkout set status = 'complete', payment_status = 'paid' where nr = 'old-paid'", C),
    z_db:q("alter table paysub_checkout drop column notified_state", C),
    z_db:flush(C),
    % Force failure after ADD COLUMN; both DDL and the marker backfill must roll back.
    z_db:q("create function reject_backfill() returns trigger language plpgsql as $$
        begin raise exception 'test backfill failure'; end $$", C),
    z_db:q("create trigger reject_backfill before update on paysub_checkout
        for each row execute function reject_backfill()", C),
    ?assertMatch({'EXIT', _}, (catch paysub_schema:install(C))),
    ?assertNot(z_db:column_exists(paysub_checkout, notified_state, C)),
    z_db:q("drop trigger reject_backfill on paysub_checkout", C),
    z_db:q("drop function reject_backfill()", C),
    ok = paysub_schema:install(C),
    ok = paysub_schema:install(C),
    ?assertEqual(lists:sort(Columns), lists:sort(z_db:column_names(paysub_checkout, C))),
    {ok, Paid} = row(<<"old-paid">>, C),
    ?assertEqual(<<"complete:paid">>, maps:get(<<"notified_state">>, Paid)),
    {ok, Pending} = row(<<"old-pending">>, C),
    ?assertEqual(undefined, maps:get(<<"notified_state">>, Pending)),
    {ok, Free} = row(<<"old-free">>, C),
    ?assertEqual(<<"complete:no_payment_required">>, maps:get(<<"notified_state">>, Free)),
    ok = m_paysub:checkout_completed(stripe, <<"old-unpaid-complete">>, C),
    {ok, OldUnpaid} = row(<<"old-unpaid-complete">>, C),
    ?assertEqual(<<"complete:unpaid">>, maps:get(<<"notified_state">>, OldUnpaid)),
    ok = m_paysub:checkout_completed(stripe, <<"old-free">>, C),
    ok = m_paysub:checkout_completed(stripe, <<"old-paid">>, C),
    no_notification(),
    Settled = (paysub_coupon_tests:session(2500))#{<<"id">> => <<"cs_old_unpaid">>,
        <<"client_reference_id">> => <<"old-unpaid-complete">>},
    ok = paysub_stripe:checkout_session_completed(Settled, C),
    notification(true),
    no_notification().
