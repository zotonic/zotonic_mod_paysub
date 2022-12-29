%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Subscriptions and payments for members using Stripe and other PSPs

%% Copyright 2022 Marc Worrell
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

-module(mod_paysub).

-mod_title("Paid subscriptions").
-mod_description("Paid subscriptions for members").
-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_depends([]).
-mod_schema(1).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    observe_search_query/2,
    observe_admin_menu/3,

    init/1,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

% -include_lib("kernel/include/logger.hrl").

observe_search_query(#search_query{ search={paysub_invoices, Args}, offsetlimit=OffsetLimit }, Context) ->
    case z_acl:is_allowed(use, mod_paysub, Context) orelse z_acl:is_admin(Context) of
        true ->
            Query = #{
                rsc_id => proplists:get_value(rsc_id, Args)
            },
            m_paysub:search_query(invoices, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_subscriptions, Args}, offsetlimit=OffsetLimit }, Context) ->
    case z_acl:is_allowed(use, mod_paysub, Context) orelse z_acl:is_admin(Context) of
        true ->
            Query = #{
                rsc_id => proplists:get_value(rsc_id, Args)
            },
            m_paysub:search_query(subscriptions, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id=paysub_admin_invoices_overview,
            parent = admin_modules,
            label = ?__("Payments - Invoices", Context),
            url = {paysub_admin_invoices_overview, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id=paysub_admin_subscriptions_overview,
            parent = admin_modules,
            label = ?__("Payments - Subscriptions", Context),
            url = {paysub_admin_subscriptions_overview, []},
            visiblecheck = {acl, use, mod_paysub}}
        | Acc
    ].



init(Context) ->
    m_config:set_default_value(?MODULE, stripe_api_key, <<>>, Context),
    m_config:set_default_value(?MODULE, stripe_webhook_secret, <<>>, Context),
    ok.

manage_schema(_Version, Context) ->
    m_paysub:install(Context).

