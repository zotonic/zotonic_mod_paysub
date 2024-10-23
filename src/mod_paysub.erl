%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022-2024 Marc Worrell
%% @doc Subscriptions and payments for members using Stripe and other PSPs
%% @end

%% Copyright 2022-2024 Marc Worrell
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
-mod_schema(13).
-mod_prio(500).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    event/2,

    observe_paysub_customer/2,

    observe_acl_user_groups_modify/3,
    observe_search_query_term/2,
    observe_search_query/2,
    observe_rsc_merge/2,
    observe_rsc_pivot_done/2,
    observe_admin_menu/3,

    observe_survey_result_columns/3,
    observe_survey_result_column_values/3,

    observe_export_resource_filename/2,
    observe_export_resource_header/2,
    observe_export_resource_data/2,
    observe_export_resource_encode/2,

    move_subscriptions/4,

    sync_products/1,

    manage_schema/2,
    manage_data/2
]).

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").
-include("../include/paysub.hrl").

event(#submit{ message = {product_update, Args} }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            {id, ProdId} = proplists:lookup(id, Args),
            UGId = z_context:get_q(<<"user_group_id">>, Context),
            IsActive = z_context:get_q(<<"is_active">>, Context),
            IsUseMainContact = z_context:get_q(<<"is_use_maincontact">>, Context),
            Update = #{
                is_active => z_convert:to_bool(IsActive),
                user_group_id => m_rsc:rid(UGId, Context),
                is_use_maincontact => z_convert:to_bool(IsUseMainContact),
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
                    z_render:wire({redirect, [{location, PortalUrl}]}, Context);
                {error, _} ->
                    z_render:growl(?__("Sorry, can't redirect to the customer portal.", Context), Context)
            end;
        _ ->
            z_render:growl(?__("Sorry, can't redirect to the customer portal.", Context), Context)
    end;
event(#postback{ message = {customer_create, Args} }, Context) ->
    {psp, PSP} = proplists:lookup(psp, Args),
    {id, Id} = proplists:lookup(id, Args),
    case m_paysub:is_allowed_paysub(Context) andalso z_acl:rsc_editable(Id, Context) of
        true ->
            case z_convert:to_binary(PSP) of
                <<"stripe">> ->
                    case paysub_stripe:ensure_stripe_customer(Id, Context) of
                        {ok, _StripeId} ->
                            z_render:wire({reload, []}, Context);
                        {error, _} ->
                            z_render:growl(?__("Sorry, something went wrong.", Context), Context)
                    end;
                _ ->
                    z_render:growl(?__("Sorry, can't create a customer for this PSP.", Context), Context)
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to create a customer for this PSP.", Context), Context)
    end;
event(#postback{ message = {customer_update, Args} }, Context) ->
    {psp, PSP} = proplists:lookup(psp, Args),
    {id, Id} = proplists:lookup(id, Args),
    case m_paysub:is_allowed_paysub(Context) andalso z_acl:rsc_editable(Id, Context) of
        true ->
            RscId = m_rsc:rid(Id, Context),
            case m_paysub:get_customer(PSP, RscId, Context) of
                {ok, _} ->
                    m_paysub:sync_billing_to_psp(Id, Context),
                    z_render:growl(?__("Data will be sent to Stripe.", Context), Context);
                {error, _} ->
                    z_render:growl(?__("Sorry, can't find the customer for this PSP.", Context), Context)
            end;
        false ->
            z_render:growl(?__("Sorry, you are not allowed to update customer for this PSP.", Context), Context)
    end;
event(#submit{ message = {set_usernamepw, Args}, form = Form }, Context) ->
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
                    % Get logon-token and redirect to the user's page or reload the current page
                    Url = case z_convert:to_binary(proplists:get_value(url, Args)) of
                        <<>> -> m_rsc:p_no_acl(UserId, page_url_abs, Context);
                        UrlArg -> UrlArg
                    end,
                    case z_notifier:first(#auth_client_logon_user{ user_id = UserId, url = Url }, Context) of
                        ok ->
                            z_render:update(Form, ?__("Redirecting...", Context), Context);
                        {error, Reason} ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Paysub could not logon new user and redirect">>,
                                result => error,
                                reason => Reason,
                                checkout_nr => CheckoutNr,
                                user_id => UserId,
                                url => Url
                            }),
                            z_render:wire({alert, [{text, ?__("Sorry, something goes wrong, try again later.", Context)}]}, Context);
                        undefined ->
                            ?LOG_ERROR(#{
                                in => zotonic_mod_paysub,
                                text => <<"Paysub could not logon new user and redirect: no handler">>,
                                result => error,
                                reason => auth_client_logon_user,
                                checkout_nr => CheckoutNr,
                                user_id => UserId,
                                url => Url
                            }),
                            z_render:wire({alert, [{text, ?__("Sorry, something goes wrong, try again later.", Context)}]}, Context)
                    end;
                {error, eexist} ->
                    z_render:wire({show, [{target, "err-username"}]}, Context);
                {error, eacces} ->
                    z_render:wire({show, [{target, "err-username"}]}, Context);
                {error, _} ->
                    z_render:wire({alert, [{text, ?__("Sorry, something goes wrong, try again later.", Context)}]}, Context)
            end;
        {ok, Status} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Password reset for checkout without expired password">>,
                result => error,
                reason => Status,
                checkout_nr => CheckoutNr,
                user_id => UserId
            }),
            z_render:wire({alert, [{text, ?__("Sorry, the password has already been reset.", Context)}]}, Context);
        {error, Reason} ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                text => <<"Password reset for checkout with error">>,
                result => error,
                reason => Reason,
                checkout_nr => CheckoutNr,
                user_id => UserId
            }),
            z_render:wire({alert, [{text, ?__("Sorry, this checkout could not be fetched.", Context)}]}, Context)
    end;
event(#submit{ message = {set_subscription_status, Args} }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            {id, SubId} = proplists:lookup(id, Args),
            Update = #{
                status => z_context:get_q(<<"status">>, Context)
            },
            m_paysub:update_subscription(SubId, Update, Context),
            OnSuccess = proplists:get_all_values(on_success, Args),
            z_render:wire(OnSuccess, Context);
        false ->
            z_render:growl(?__("You are not allowed to edit products.", Context), Context)
    end;
event(#postback{ message = {move_subscriptions, Args} }, Context) ->
    {from_id, FromId} = proplists:lookup(from_id, Args),
    {to_id, ToId} = proplists:lookup(to_id, Args),
    IsOnlyMainContact = z_convert:to_bool(proplists:get_value(is_only_maincontact, Args)),
    case z_acl:rsc_editable(FromId, Context) andalso z_acl:rsc_editable(ToId, Context) of
        true ->
            case lists:member(FromId, m_edge:objects(ToId, hasmaincontact, Context)) of
                true ->
                    move_subscriptions(FromId, ToId, IsOnlyMainContact, Context),
                    z_render:wire(proplists:get_value(on_success, Args), Context);
                false ->
                    z_render:growl_error(?__("The user is not a main contact of the organization.", Context), Context)
            end;
        false ->
            z_render:growl(?__("You are not allowed to do this.", Context), Context)
    end;
event(#postback{ message = {products_fetch, _Args} }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            sync_products(Context),
            z_render:wire({reload, []}, Context);
        false ->
            Context1 = z_render:wire({unmask, [ {target, " body"} ]}, Context),
            z_render:growl(?__("You are not allowed to edit products.", Context), Context1)
    end.


%% @doc Observe updates to the billing address at the PSP after sync to the customer.
%% Ensure that the address fields in the rsc billing address is the same as the customer
%% fields. If the config mod_paysub.is_no_customer_sync is set, then the resource will
%% not be updated.
observe_paysub_customer(#paysub_customer{ action = delete }, _Context) ->
    undefined;
observe_paysub_customer(#paysub_customer{ customer = #{ <<"rsc_id">> := undefined } }, _Context) ->
    undefined;
observe_paysub_customer(#paysub_customer{ customer = #{ <<"rsc_id">> := RscId } = Cust }, Context) ->
    case m_config:get_boolean(mod_paysub, is_no_customer_sync, Context) of
        true ->
            undefined;
        false ->
            case m_paysub:billing_address(RscId, Context) of
                {ok, Billing} ->
                    Diff = billing_changed(Cust, Billing),
                    case maps:size(Diff) of
                        0 ->
                            ok;
                        _ ->
                            ?LOG_INFO(#{
                                in => zotonic_mod_paysub,
                                text => <<"Synchronizing billing information from psp to resource">>,
                                result => ok,
                                rsc_id => RscId,
                                psp => maps:get(<<"psp">>, Cust),
                                psp_customer_id => maps:get(<<"psp_customer_id">>, Cust),
                                billing => Diff
                            }),
                            m_rsc:update(RscId, Diff, [ no_touch ], z_acl:sudo(Context))
                    end
            end
    end.

% Collect the changed billing address.
billing_changed(Cust, Billing) ->
    {Upd, UpdIsAdr} = lists:foldl(
        fun({C, B, IsAdr}, {Acc, AccIsAdr}) ->
            CV = z_convert:to_binary(maps:get(C, Cust, <<>>)),
            BV = z_convert:to_binary(maps:get(B, Billing, <<>>)),
            if
                CV =:= BV ->
                    {Acc, AccIsAdr};
                true ->
                    {Acc#{ <<"billing_", B/binary>> => CV }, AccIsAdr orelse IsAdr}
            end
        end,
        {#{}, false},
        [
            {<<"name">>, <<"name">>, false},
            {<<"email">>, <<"email">>, false},
            {<<"phone">>, <<"phone">>, false},
            {<<"address_country">>, <<"country">>, true},
            {<<"address_city">>, <<"city">>, true},
            {<<"address_street_1">>, <<"street_1">>, true},
            {<<"address_street_2">>, <<"street_2">>, true},
            {<<"address_postcode">>, <<"postcode">>, true},
            {<<"address_state">>, <<"state">>, true}
        ]),
    if
        UpdIsAdr ->
            % Always set the complete billing address if any billing address part
            % is changed.
            Upd#{
                <<"billing_country">> => maps:get(<<"address_country">>, Cust, undefined),
                <<"billing_city">> => maps:get(<<"address_city">>, Cust, undefined),
                <<"billing_street_1">> => maps:get(<<"address_street_1">>, Cust, undefined),
                <<"billing_street_2">> => maps:get(<<"address_street_2">>, Cust, undefined),
                <<"billing_postcode">> => maps:get(<<"address_postcode">>, Cust, undefined),
                <<"billing_state">> => maps:get(<<"address_state">>, Cust, undefined)
            };
        true ->
            Upd
    end.


%% @doc Modify the user group based on the active subscriptions. If no subscription found
%% Then the group `ug_inactive_member` is added. Called by mod_acl_user_groups when a new
%% user context is initialized.
-spec observe_acl_user_groups_modify(#acl_user_groups_modify{}, UGs, Context) -> NewUGs when
    UGs :: list( m_rsc:resource_id() ),
    Context :: z:context(),
    NewUGs :: list( m_rsc:resource_id() ).
observe_acl_user_groups_modify(#acl_user_groups_modify{ id = undefined }, Groups, _Context) ->
    Groups;
observe_acl_user_groups_modify(#acl_user_groups_modify{ id = UserId }, Groups, Context) ->
    ContextAsync = z_context:prune_for_async(Context),
    MainContactOf = m_edge:subjects(UserId, hasmaincontact, Context),
    UGs = m_paysub:users_groups([ UserId | MainContactOf ], ContextAsync)
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
    m_paysub_search:search_query_term(Term, Context).

observe_search_query(#search_query{ search={paysub_invoices, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                <<"rsc_id">> => proplists:get_value(rsc_id, Args)
            },
            m_paysub_search:search_query(invoices, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_subscriptions, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                <<"rsc_id">> => proplists:get_value(rsc_id, Args),
                <<"price_id">> => proplists:get_value(price_id, Args),
                <<"product_id">> => proplists:get_value(product_id, Args),
                <<"status">> => proplists:get_value(status, Args),
                <<"user_group_id">> => proplists:get_value(user_group_id, Args),
                <<"psp">> => proplists:get_value(psp, Args),
                <<"country">> => proplists:get_value(country, Args),
                <<"city">> => proplists:get_value(city, Args),
                <<"postcode">> => proplists:get_value(postcode, Args)
            },
            m_paysub_search:search_query(subscriptions, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_products, _Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{},
            m_paysub_search:search_query(products, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{ search={paysub_payments, Args}, offsetlimit=OffsetLimit }, Context) ->
    case m_paysub:is_allowed_paysub(Context) of
        true ->
            Query = #{
                <<"rsc_id">> => proplists:get_value(rsc_id, Args)
            },
            m_paysub_search:search_query(payments, Query, OffsetLimit, Context);
        false ->
            []
    end;
observe_search_query(#search_query{}, _Context) ->
    undefined.

observe_rsc_merge(#rsc_merge{ winner_id = WinnerId, loser_id = LoserId }, Context) ->
    m_paysub:rsc_merge(WinnerId, LoserId, Context),
    ok.

%% @doc Move subscriptions from one resource to another. After the
%% customers and subscriptions have been moved, the customer details are
%% synced to the PSP.
move_subscriptions(FromId, ToId, IsOnlyMainContact, Context) ->
    m_paysub:move_subscriptions(FromId, ToId, IsOnlyMainContact, Context),
    m_paysub:sync_customer_rsc_id(ToId, Context),
    ok.

observe_rsc_pivot_done(#rsc_pivot_done{ id = Id }, Context) ->
    m_paysub:sync_billing_to_psp(Id, Context).

observe_admin_menu(#admin_menu{}, Acc, Context) ->
    [
        #menu_item{
            id = paysub_admin_dashboard,
            parent = admin_modules,
            label = ?__("Payments - Dashboard", Context),
            url = {paysub_admin_dashboard, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id = paysub_admin_invoices_overview,
            parent = admin_modules,
            label = ?__("Payments - Invoices", Context),
            url = {paysub_admin_invoices_overview, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id = paysub_admin_subscriptions_overview,
            parent = admin_modules,
            label = ?__("Payments - Subscriptions", Context),
            url = {paysub_admin_subscriptions_overview, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id = paysub_admin_products_overview,
            parent = admin_modules,
            label = ?__("Payments - Products", Context),
            url = {paysub_admin_products_overview, []},
            visiblecheck = {acl, use, mod_paysub}},
        #menu_item{
            id = paysub_admin_payments_overview,
            parent = admin_modules,
            label = ?__("Payments - Payments", Context),
            url = {paysub_admin_payments_overview, []},
            visiblecheck = {acl, use, mod_paysub}}
        | Acc
    ].


observe_survey_result_columns(#survey_result_columns{ id = Id }, Cols, Context) ->
    case m_paysub:is_survey_checkout(Id, Context) of
        true ->
            Cols ++ [ {<<"paysub_status">>, ?__("Payment status", Context)} ];
        false ->
            Cols
    end.

observe_survey_result_column_values(#survey_result_column_values{
        id = SurveyId,
        answer = Answer,
        format = Format,
        columns = Cols
    }, Vs, Context) ->
    case proplists:is_defined(<<"paysub_status">>, Cols) of
        true ->
            AnswerId = proplists:get_value(id, Answer),
            Checkout = case m_paysub:survey_checkout_status(SurveyId, AnswerId, Context) of
                {ok, C} ->
                    C;
                {error, _} ->
                    undefined
            end,
            % Add payment status for this view
            Vars = [
                {survey_id, SurveyId},
                {answer, Answer},
                {checkout, Checkout},
                {format, Format}
            ],
            {Html, _} = z_template:render_to_iolist(<<"_survey_paysub_checkout_status.tpl">>, Vars, Context),
            Html1 = z_string:trim(iolist_to_binary(Html)),
            Html2 = binary:replace(Html1, [ <<10>>, <<9>>, <<13>> ], <<" ">>, [ global ]),
            Vs#{
                <<"paysub_status">> => Html2
            };
        false ->
            Vs
    end.


%% @doc Export data
observe_export_resource_filename(#export_resource_filename{ dispatch = paysub_export_subscriptions }, Context) ->
    paysub_export_subscriptions:filename(Context);
observe_export_resource_filename(_, _) ->
    undefined.

observe_export_resource_header(#export_resource_header{ dispatch = paysub_export_subscriptions }, Context) ->
    paysub_export_subscriptions:header(Context);
observe_export_resource_header(_, _) ->
    undefined.

observe_export_resource_data(#export_resource_data{ dispatch = paysub_export_subscriptions, state = State }, Context) ->
    paysub_export_subscriptions:data(State, Context);
observe_export_resource_data(_, _) ->
    undefined.

observe_export_resource_encode(#export_resource_encode{ dispatch = paysub_export_subscriptions, state = State, data = Data }, Context) ->
    paysub_export_subscriptions:encode(Data, State, Context);
observe_export_resource_encode(_, _) ->
    undefined.


%% @doc Fetch all products and prices from all payment service providers.
-spec sync_products(Context) -> ok | {error, Reason} when
    Context :: z:context(),
    Reason :: term().
sync_products(Context) ->
    case paysub_stripe:is_configured(Context) of
        true ->
            case paysub_stripe:sync_products(Context) of
                ok -> paysub_stripe:sync_prices(Context);
                {error, _} = Error -> Error
            end;
        false ->
            ok
    end.


manage_schema(_Version, Context) ->
    ok = paysub_schema:install(Context),
    case m_rsc:rid(acl_user_group, Context) of
        undefined ->
            #datamodel{};
        _UG ->
            #datamodel{
                resources = [
                    {ug_inactive_member, acl_user_group, #{
                        <<"language">> => [ en, de ],
                        <<"title">> => #trans{ tr = [
                            {en, <<"Members without subscriptions">>},
                            {nl, <<"Leden zonder abonnement">>},
                            {de, <<"Mitglieder ohne Abonnement"/utf8>>}
                        ]}
                    }}
                ]}
    end.

manage_data(_Version, Context) ->
    m_config:set_default_value(mod_paysub, stripe_api_key, <<>>, Context),
    m_config:set_default_value(mod_paysub, stripe_webhook_secret, <<>>, Context).
