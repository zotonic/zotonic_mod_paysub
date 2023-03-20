%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022-2023 Marc Worrell
%% @doc Subscriptions and payments for members using Stripe and other PSPs

%% Copyright 2022-2023 Marc Worrell
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
-mod_depends([ mod_authentication ]).
-mod_schema(2).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    event/2,

    observe_acl_user_groups_modify/3,
    observe_search_query_term/2,
    observe_search_query/2,
    observe_rsc_merge/2,
    observe_admin_menu/3,

    init/1,
    manage_schema/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

% -include_lib("kernel/include/logger.hrl").

event(#submit{ message = {product_update, Args} }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            {id, ProdId} = proplists:lookup(id, Args),
            UGId = z_context:get_q(<<"user_group_id">>, Context),
            IsActive = z_context:get_q(<<"is_active">>, Context),
            Update = #{
                is_active => z_convert:to_bool(IsActive),
                user_group_id => m_rsc:rid(UGId, Context),
                modified => calendar:universal_time()
            },
            m_paysub:update_product(ProdId, Update, Context),
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(OnSuccess, Context);
        false ->
            z_render:growl(?__("You are not allowed to edit products.", Context), Context)
    end;
event(#postback{ message = {customer_portal, Args} }, Context) ->
    ReturnUrl = proplists:get_value(return_url, Args),
    case z_convert:to_binary(proplists:get_value(psp, Args)) of
        <<"stripe">> ->
            case paysub_stripe:customer_portal_session_url(z_acl:user(Context), ReturnUrl, Context) of
                {ok, PortalUrl} ->
                    z_render:wire({redirect, [{url, PortalUrl}]}, Context);
                {error, _} ->
                    z_render:growl(?__("Sorry, can't redirect to the customer portal.", Context), Context)
            end;
        _ ->
            z_render:growl(?__("Sorry, can't redirect to the customer portal.", Context), Context)
    end;
event(#submit{ message = {set_usernamepw, Args} }, Context) ->
    {checkout_nr, CheckoutNr} = proplists:lookup(checkout_nr, Args),
    {user_id, UserId} = proplists:lookup(user_id, Args),
    case m_paysub:checkout_status(CheckoutNr, Context) of
        {ok, #{
            <<"user_info">> := #{
                <<"user_id">> := UserId,
                <<"visited">> := undefined,
                <<"is_expired">> := true
            }
        }} ->
            Username = z_context:get_q_validated(<<"username">>, Context),
            Password = z_context:get_q_validated(<<"password">>, Context),
            case m_identity:set_username_pw(UserId, Username, Password, z_acl:sudo(Context)) of
                ok ->
                    % Get logon-token and redirect to the user's page
                    Url = m_rsc:p(UserId, page_url_abs, Context),
                    case z_notifier:first(#auth_client_logon_user{ user_id = UserId, url = Url }, Context) of
                        ok ->
                            Context;
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => mod_paysub,
                                text => <<"Paysub could not logon new user and redirect">>,
                                result => error,
                                reason => Reason,
                                checkout_nr => CheckoutNr,
                                user_id => UserId,
                                url => Url
                            }),
                            z_render:wire({alert, [{text, ?__("Sorry, something goed wrong, try again later.", Context)}]}, Context);
                        undefined ->
                            ?LOG_ERROR(#{
                                in => mod_paysub,
                                text => <<"Paysub could not logon new user and redirect: no handler">>,
                                result => error,
                                reason => auth_client_logon_user,
                                checkout_nr => CheckoutNr,
                                user_id => UserId,
                                url => Url
                            }),
                            z_render:wire({alert, [{text, ?__("Sorry, something goed wrong, try again later.", Context)}]}, Context)
                    end;
                {error, eexist} ->
                    z_render:wire({show, [{target, "err-username"}]}, Context);
                {error, _} ->
                    z_render:wire({alert, [{text, ?__("Sorry, something goed wrong, try again later.", Context)}]}, Context)
            end;
        {ok, Status} ->
            ?LOG_ERROR(#{
                in => mod_paysub,
                text => <<"Password reset for checkout without expired password">>,
                result => error,
                reason => Status,
                checkout_nr => CheckoutNr,
                user_id => UserId
            }),
            z_render:wire({alert, [{text, ?__("Sorry, something goed wrong, try again later.", Context)}]}, Context);
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => mod_paysub,
                text => <<"Password reset for checkout with error">>,
                result => error,
                reason => Reason,
                checkout_nr => CheckoutNr,
                user_id => UserId
            }),
            z_render:wire({alert, [{text, ?__("Sorry, this checkout could not be fetched.", Context)}]}, Context)
    end.


%% @doc Modify the user group based on the active subscriptions. If no subscription found
%% Then the group `ug_inactive_member` is added.
observe_acl_user_groups_modify(#acl_user_groups_modify{ id = undefined }, Groups, _Context) ->
    Groups;
observe_acl_user_groups_modify(#acl_user_groups_modify{ id = UserId }, Groups, Context) ->
    MainContactOf = m_edge:subjects(UserId, hasmaincontact, Context),
    UGs = m_paysub:users_groups([ UserId | MainContactOf ], Context)
          ++ main_contact_ugs(MainContactOf, Context),
    Groups1 = case UGs of
        [] ->
            [ m_rsc:rid(ug_inactive_member, Context) | Groups ];
        _ ->
            UGs ++ Groups
    end,
    lists:usort(Groups1).

main_contact_ugs(Ids, Context) ->
    lists:flatten(
        lists:map(
            fun(Id) ->
                m_edge:objects(Id, hasusergroup, Context)
            end,
            Ids)).

observe_search_query_term(#search_query_term{} = Term, Context) ->
    m_paysub:search_query_term(Term, Context).

observe_search_query(#search_query{ search={paysub_invoices, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                rsc_id => proplists:get_value(rsc_id, Args)
            },
            m_paysub:search_query(invoices, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_subscriptions, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                rsc_id => proplists:get_value(rsc_id, Args),
                price_id => proplists:get_value(price_id, Args),
                product_id => proplists:get_value(product_id, Args)
            },
            m_paysub:search_query(subscriptions, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_products, _Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{},
            m_paysub:search_query(products, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_payments, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                rsc_id => proplists:get_value(rsc_id, Args)
            },
            m_paysub:search_query(payments, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_rsc_merge(#rsc_merge{ winner_id = WinnerId, loser_id = LoserId }, Context) ->
    m_paysub:rsc_merge(WinnerId, LoserId, Context),
    ok.

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
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id=paysub_admin_products_overview,
            parent = admin_modules,
            label = ?__("Payments - Products", Context),
            url = {paysub_admin_products_overview, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id=paysub_admin_payments_overview,
            parent = admin_modules,
            label = ?__("Payments - Payments", Context),
            url = {paysub_admin_payments_overview, []},
            visiblecheck = {acl, use, mod_paysub}}
        | Acc
    ].



init(Context) ->
    m_config:set_default_value(?MODULE, stripe_api_key, <<>>, Context),
    m_config:set_default_value(?MODULE, stripe_webhook_secret, <<>>, Context),
    ok.

manage_schema(_Version, Context) ->
    ok = m_paysub:install(Context),
    #datamodel{
        resources = [
            {ug_inactive_member, acl_user_group, #{
                <<"language">> => [ en, de ],
                <<"title">> => #trans{ tr = [
                    {en, <<"Members without subscriptions">>},
                    {de, <<"Mitglieder ohne Abonnement"/utf8>>}
                ]}
            }}
        ]}.

