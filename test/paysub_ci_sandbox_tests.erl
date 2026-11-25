%% @copyright 2026 Marc Worrell
%% @doc CI smoke test: activate paysub and install its schema in a real site.
-module(paysub_ci_sandbox_tests).
-include_lib("eunit/include/eunit.hrl").

sandbox_test_() ->
    case os:getenv("PAYSUB_CI") of
        false -> [];
        _ -> {timeout, 120, fun install/0}
    end.

install() ->
    Context = z_acl:sudo(z_context:new(zotonic_site_testsandbox)),
    ?assertEqual(ok, z_module_manager:activate_await(mod_paysub, Context)),
    ?assert(z_module_manager:active(mod_paysub, Context)),
    ?assert(z_db:table_exists(paysub_checkout, Context)),
    ?assert(z_db:column_exists(paysub_checkout, notified_state, Context)),
    ?assert(z_db:table_exists(paysub_payment, Context)),
    ?assert(z_db:table_exists(paysub_subscription, Context)),
    ?assert(z_db:table_exists(paysub_invoice, Context)).
