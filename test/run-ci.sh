#!/usr/bin/env bash
# Run all paysub tests, including database regressions, in a live Zotonic sandbox.
set -euo pipefail
cd "$(dirname "$0")/../../.."
: "${PAYMENT_TEST_PORT:?Set PAYMENT_TEST_PORT so DB tests cannot silently skip}"
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/paysub-ci.XXXXXX")
trap 'rm -rf "$build_dir"' EXIT
export ERL_LIBS="$PWD/_build/default/lib"
export ZOTONIC="$PWD"
export PAYSUB_CI=1

sources=()
while IFS= read -r -d '' source; do sources+=("$source"); done < <(
    find apps_user/zotonic_mod_paysub/src -name '*.erl' -print0
)
# The src include path also resolves the model's ../include/paysub.hrl.
erlc +debug_info -I apps -I apps_user -I apps_user/zotonic_mod_paysub/src \
    -o "$build_dir" "${sources[@]}" apps_user/zotonic_mod_paysub/test/*.erl

erl -pa "$PWD"/_build/default/lib/*/ebin -pa "$build_dir" \
    -sname zotonic001_testsandbox -sasl errlog_type error -s zotonic -noshell \
    -eval 'spawn(fun() -> receive after 300000 -> io:format(standard_error, "Sandbox tests timed out~n", []), halt(1) end end),
        Files = filelib:wildcard("apps_user/zotonic_mod_paysub/test/*_tests.erl"),
        Tests = [list_to_atom(filename:basename(F, ".erl")) || F <- Files],
        HealthCheck = fun() ->
            C = z_context:new(zotonic_site_testsandbox),
            1 = z_db:q1("select 1", C),
            true = z_module_manager:active(mod_paysub, C)
        end,
        zotonic:runtests([paysub_ci_sandbox_tests | Tests -- [paysub_ci_sandbox_tests]] ++ [HealthCheck]).'
