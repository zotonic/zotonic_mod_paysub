%% @copyright 2022 Marc Worrell
%% @doc Handle stripe callbacks for subscription and payment events
%% @end

%% Copyright 2022 Marc Worrrell
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

-module(controller_paysub_stripe_webhook).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    allowed_methods/1,
    is_authorized/1,
    process/4
]).

-define(TIMESTAMP_TOLERANCE, 10).

-include_lib("zotonic_core/include/zotonic.hrl").

allowed_methods(Context) ->
    {[ <<"POST">> ], Context}.

is_authorized(Context) ->
    {Body, Context1} = cowmachine_req:req_body(Context),
    case is_valid_signature(Body, Context1) of
        true ->
            {true, z_context:set(body, Body, Context1)};
        false ->
            ?LOG_ERROR(#{
                in => zotonic_mod_paysub,
                psp => stripe,
                text => <<"Stripe webhook: rejected secret.">>,
                result => error,
                reason => webhook_secret
            }),
            {<<"Stripe-Webhook-Secret">>, Context1}
    end.

process(<<"POST">>, _AcceptedCT, _ProvidedCT, Context) ->
    Parsed = #{ <<"type">> := EventType } = z_json:decode(z_context:get(body, Context)),
    ?DEBUG(EventType),
    case handle(EventType, Parsed, Context) of
        ok ->
            {true, Context};
        {ok, _} ->
            {true, Context};
        % {error, session_data} ->
        %     {{halt, 404}, Context};
        {error, _} ->
            {{halt, 500}, Context}
    end.

handle(<<"ping">>, _Ps, _Context) ->
    ?LOG_DEBUG(#{
        in => zotonic_mod_paysub,
        psp => stripe,
        text => <<"Stripe ping">>,
        result => ok,
        event_type => <<"ping">>
    }),
    ok;
handle(<<"checkout.session.async_payment_failed">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(<<"checkout.session.async_payment_succeeded">>, Payload, Context) ->
    sync_session(Payload, Context);
handle(<<"checkout.session.completed">>, Payload, Context) ->
    % Payment is successful and the subscription is created.
    % You should provision the subscription and save the customer ID to your database.
    sync_session(Payload, Context);
handle(<<"checkout.session.expired">>, Payload, Context) ->
    sync_session(Payload, Context);

handle(<<"invoice.created">>, Payload, Context) ->
    sync_invoice(Payload, Context);
handle(<<"invoice.updated">>, Payload, Context) ->
    sync_invoice(Payload, Context);
handle(<<"invoice.voided">>, Payload, Context) ->
    sync_invoice(Payload, Context);
handle(<<"invoice.finalized">>, Payload, Context) ->
    sync_invoice(Payload, Context);
handle(<<"invoice.paid">>, Payload, Context) ->
    % Continue to provision the subscription as payments continue to be made.
    % Store the status in your database and check when a user accesses your service.
    % This approach helps you avoid hitting rate limits.
    sync_invoice(Payload, Context);
handle(<<"invoice.payment_failed">>, Payload, Context) ->
    % The payment failed or the customer does not have a valid payment method.
    % The subscription becomes 'past_due'. Notify your customer and send them to the
    % customer portal to update their payment information.
    sync_invoice(Payload, Context);
handle(<<"invoice.deleted">>, Payload, Context) ->
    delete_invoice(Payload, Context);
handle(<<"invoice.", _/binary>>, _Payload, _Context) ->
    ok;

%     when 'invoice.finalization_failed'
%         invoice = event.data.object
%     when 'invoice.marked_uncollectible'
%         invoice = event.data.object
%     when 'invoice.payment_action_required'
%         invoice = event.data.object
%     when 'invoice.payment_succeeded'
%         invoice = event.data.object
%     when 'invoice.sent'
%         invoice = event.data.object
%     when 'invoice.upcoming'
%         invoice = event.data.object

handle(<<"customer.created">>, Ps, Context) ->
    sync_customer(Ps, Context);
handle(<<"customer.updated">>, Ps, Context) ->
    sync_customer(Ps, Context);
handle(<<"customer.deleted">>, Ps, Context) ->
    delete_customer(Ps, Context);

handle(<<"customer.subscription.created">>, Ps, Context) ->
    sync_subscription(Ps, Context);
handle(<<"customer.subscription.updated">>, Ps, Context) ->
    sync_subscription(Ps, Context);
handle(<<"customer.subscription.deleted">>, Ps, Context) ->
    sync_subscription(Ps, Context);
handle(<<"customer.", _/binary>> = Type, Ps, _Context) ->
    ?DEBUG(Type),
    ?DEBUG(Ps),
    ok;

handle(<<"price.", _/binary>>, _Ps, Context) ->
    paysub_stripe:schedule_sync(prices, Context),
    ok;
handle(<<"product.", _/binary>>, _Ps, Context) ->
    paysub_stripe:schedule_sync(products, Context),
    ok;

handle(<<"payment_intent.", _/binary>>, Ps, Context) ->
    sync_payment(Ps, Context);

handle(Type, _Ps, _Context) ->
    % Unhandled event type
    ?LOG_INFO(#{
        in => zotonic_mod_paysub,
        psp => stripe,
        text => <<"Unhandled Stripe event type">>,
        result => error,
        reason => unhandled_event,
        event_type => Type
    }),
    ok.

sync_session(#{
        <<"object">> := <<"event">>,
        <<"data">> := #{
            <<"object">> := #{
                <<"object">> := <<"checkout.session">>,
                <<"id">> := SessionId
            }
        }
    }, Context) ->
    paysub_stripe:checkout_session_sync(SessionId, Context);
sync_session(Payload, _Context) ->
    ?LOG_ERROR(#{
        in => zotonic_mod_paysub,
        psp => stripe,
        text => <<"Unknown payload when processing Stripe webhook data">>,
        result => error,
        reason => payload,
        payload => Payload
    }),
    {error, payload}.

sync_customer(#{ <<"data">> := #{
        <<"object">> := #{
            <<"id">> := CustId,
            <<"object">> := <<"customer">>
        }
    }}, Context) ->
    UniqueKey = <<"paysub-stripe-", CustId/binary>>,
    z_pivot_rsc:insert_task_after(5, paysub_stripe, sync_task, UniqueKey, [ customer, CustId ], Context),
    Args = [
        CustId,
        z_context:site(Context)
    ],
    buffalo:queue({paysub_stripe, sync_customer, Args}, #{ timeout => 100, deadline => 250 }).

delete_customer(#{ <<"data">> := #{ <<"object">> := Cust }}, Context) ->
    _ = paysub_stripe:delete_customer(Cust, Context),
    ok.

sync_subscription(#{
    <<"data">> := #{
        <<"object">> := #{
            <<"id">> := SubId,
            <<"object">> := <<"subscription">>
        }
    }}, Context) ->
    UniqueKey = <<"paysub-stripe-", SubId/binary>>,
    z_pivot_rsc:insert_task_after(5, paysub_stripe, sync_task, UniqueKey, [ subscription, SubId ], Context),
    Args = [
        SubId,
        z_context:site(Context)
    ],
    buffalo:queue({paysub_stripe, sync_subscription, Args}, #{ timeout => 100, deadline => 250 }).

% delete_subscription(#{ <<"data">> := #{ <<"object">> := Sub }}, Context) ->
%     _ = paysub_stripe:delete_subscription(Sub, Context),
%     ok.

delete_invoice(#{ <<"data">> := #{ <<"object">> := Sub }}, Context) ->
    _ = paysub_stripe:delete_invoice(Sub, Context),
    ok.

sync_payment(#{ <<"data">> := #{
        <<"object">> := #{
            <<"id">> := PaymentId,
            <<"object">> := <<"payment_intent">>
        }
    }}, Context) ->
    UniqueKey = <<"paysub-stripe-", PaymentId/binary>>,
    z_pivot_rsc:insert_task_after(5, paysub_stripe, sync_task, UniqueKey, [ payment, PaymentId ], Context),
    Args = [
        PaymentId,
        z_context:site(Context)
    ],
    buffalo:queue({paysub_stripe, sync_payment, Args}, #{ timeout => 1000, deadline => 2000 }).

sync_invoice(#{ <<"data">> := #{
        <<"object">> := #{
            <<"id">> := InvId,
            <<"object">> := <<"invoice">>
        }
    }}, Context) ->
    UniqueKey = <<"paysub-stripe-", InvId/binary>>,
    z_pivot_rsc:insert_task_after(10, paysub_stripe, sync_task, UniqueKey, [ invoice, InvId ], Context),
    Args = [
        InvId,
        z_context:site(Context)
    ],
    buffalo:queue({paysub_stripe, sync_invoice, Args}, #{ timeout => 1000, deadline => 2000 }).


is_valid_signature(Body, Context) ->
    case z_context:get_req_header(<<"stripe-signature">>, Context) of
        undefined ->
            false;
        <<>> ->
            false;
        Sig ->
            Ps = lists:map(
                fun(P) ->
                    [A,B] = binary:split(P, <<"=">>),
                    {A,B}
                end,
                binary:split(Sig, <<",">>, [ global ])),
            T = proplists:get_value(<<"t">>, Ps),
            V1 = proplists:get_value(<<"v1">>, Ps),
            Key = m_config:get_value(mod_paysub, stripe_webhook_secret, Context),
            MySig = crypto:mac(hmac, sha256, Key, <<T/binary, ".", Body/binary>>),
            MySigHex = z_string:to_lower(z_utils:hex_encode(MySig)),
            TV = binary_to_integer(T),
            Now = z_datetime:timestamp(),
            (MySigHex =:= V1 andalso abs(Now - TV) < ?TIMESTAMP_TOLERANCE)
    end.

% # server.rb
% #
% # Use this sample code to handle webhook events in your integration.
% #
% # 1) Paste this code into a new file (server.rb)
% #
% # 2) Install dependencies
% #   gem install sinatra
% #   gem install stripe
% #
% # 3) Run the server on http://localhost:4242
% #   ruby server.rb

% require 'json'
% require 'sinatra'
% require 'stripe'

% # This is your Stripe CLI webhook secret for testing your endpoint locally.
% endpoint_secret = 'whsec_32b7ced96a8fb5f2ef84afaa8234f25f84fcf1e1b97ab811c7d34ce540e8f1e5'

% set :port, 4242

% post '/webhook' do
%     payload = request.body.read
%     sig_header = request.env['HTTP_STRIPE_SIGNATURE']
%     event = nil

%     begin
%         event = Stripe::Webhook.construct_event(
%             payload, sig_header, endpoint_secret
%         )
%     rescue JSON::ParserError => e
%         # Invalid payload
%         status 400
%         return
%     rescue Stripe::SignatureVerificationError => e
%         # Invalid signature
%         status 400
%         return
%     end

%     # Handle the event
%     case event.type
%     when 'account.updated'
%         account = event.data.object
%     when 'account.external_account.created'
%         external_account = event.data.object
%     when 'account.external_account.deleted'
%         external_account = event.data.object
%     when 'account.external_account.updated'
%         external_account = event.data.object
%     when 'balance.available'
%         balance = event.data.object
%     when 'billing_portal.configuration.created'
%         configuration = event.data.object
%     when 'billing_portal.configuration.updated'
%         configuration = event.data.object
%     when 'billing_portal.session.created'
%         session = event.data.object
%     when 'capability.updated'
%         capability = event.data.object
%     when 'cash_balance.funds_available'
%         cash_balance = event.data.object
%     when 'charge.captured'
%         charge = event.data.object
%     when 'charge.expired'
%         charge = event.data.object
%     when 'charge.failed'
%         charge = event.data.object
%     when 'charge.pending'
%         charge = event.data.object
%     when 'charge.refunded'
%         charge = event.data.object
%     when 'charge.succeeded'
%         charge = event.data.object
%     when 'charge.updated'
%         charge = event.data.object
%     when 'charge.dispute.closed'
%         dispute = event.data.object
%     when 'charge.dispute.created'
%         dispute = event.data.object
%     when 'charge.dispute.funds_reinstated'
%         dispute = event.data.object
%     when 'charge.dispute.funds_withdrawn'
%         dispute = event.data.object
%     when 'charge.dispute.updated'
%         dispute = event.data.object
%     when 'charge.refund.updated'
%         refund = event.data.object
%     when 'checkout.session.async_payment_failed'
%         session = event.data.object
%     when 'checkout.session.async_payment_succeeded'
%         session = event.data.object
%     when 'checkout.session.completed'
%         session = event.data.object
%     when 'checkout.session.expired'
%         session = event.data.object
%     when 'coupon.created'
%         coupon = event.data.object
%     when 'coupon.deleted'
%         coupon = event.data.object
%     when 'coupon.updated'
%         coupon = event.data.object
%     when 'credit_note.created'
%         credit_note = event.data.object
%     when 'credit_note.updated'
%         credit_note = event.data.object
%     when 'credit_note.voided'
%         credit_note = event.data.object
%     when 'customer.created'
%         customer = event.data.object
%     when 'customer.deleted'
%         customer = event.data.object
%     when 'customer.updated'
%         customer = event.data.object
%     when 'customer.discount.created'
%         discount = event.data.object
%     when 'customer.discount.deleted'
%         discount = event.data.object
%     when 'customer.discount.updated'
%         discount = event.data.object
%     when 'customer.source.created'
%         source = event.data.object
%     when 'customer.source.deleted'
%         source = event.data.object
%     when 'customer.source.expiring'
%         source = event.data.object
%     when 'customer.source.updated'
%         source = event.data.object
%     when 'customer.subscription.created'
%         subscription = event.data.object
%     when 'customer.subscription.deleted'
%         subscription = event.data.object
%     when 'customer.subscription.pending_update_applied'
%         subscription = event.data.object
%     when 'customer.subscription.pending_update_expired'
%         subscription = event.data.object
%     when 'customer.subscription.trial_will_end'
%         subscription = event.data.object
%     when 'customer.subscription.updated'
%         subscription = event.data.object
%     when 'customer.tax_id.created'
%         tax_id = event.data.object
%     when 'customer.tax_id.deleted'
%         tax_id = event.data.object
%     when 'customer.tax_id.updated'
%         tax_id = event.data.object
%     when 'customer_cash_balance_transaction.created'
%         customer_cash_balance_transaction = event.data.object
%     when 'file.created'
%         file = event.data.object
%     when 'financial_connections.account.created'
%         account = event.data.object
%     when 'financial_connections.account.deactivated'
%         account = event.data.object
%     when 'financial_connections.account.disconnected'
%         account = event.data.object
%     when 'financial_connections.account.reactivated'
%         account = event.data.object
%     when 'financial_connections.account.refreshed_balance'
%         account = event.data.object
%     when 'identity.verification_session.canceled'
%         verification_session = event.data.object
%     when 'identity.verification_session.created'
%         verification_session = event.data.object
%     when 'identity.verification_session.processing'
%         verification_session = event.data.object
%     when 'identity.verification_session.requires_input'
%         verification_session = event.data.object
%     when 'identity.verification_session.verified'
%         verification_session = event.data.object
%     when 'invoice.created'
%         invoice = event.data.object
%     when 'invoice.deleted'
%         invoice = event.data.object
%     when 'invoice.finalization_failed'
%         invoice = event.data.object
%     when 'invoice.finalized'
%         invoice = event.data.object
%     when 'invoice.marked_uncollectible'
%         invoice = event.data.object
%     when 'invoice.paid'
%         invoice = event.data.object
%     when 'invoice.payment_action_required'
%         invoice = event.data.object
%     when 'invoice.payment_failed'
%         invoice = event.data.object
%     when 'invoice.payment_succeeded'
%         invoice = event.data.object
%     when 'invoice.sent'
%         invoice = event.data.object
%     when 'invoice.upcoming'
%         invoice = event.data.object
%     when 'invoice.updated'
%         invoice = event.data.object
%     when 'invoice.voided'
%         invoice = event.data.object
%     when 'invoiceitem.created'
%         invoiceitem = event.data.object
%     when 'invoiceitem.deleted'
%         invoiceitem = event.data.object
%     when 'invoiceitem.updated'
%         invoiceitem = event.data.object
%     when 'issuing_authorization.created'
%         issuing_authorization = event.data.object
%     when 'issuing_authorization.updated'
%         issuing_authorization = event.data.object
%     when 'issuing_card.created'
%         issuing_card = event.data.object
%     when 'issuing_card.updated'
%         issuing_card = event.data.object
%     when 'issuing_cardholder.created'
%         issuing_cardholder = event.data.object
%     when 'issuing_cardholder.updated'
%         issuing_cardholder = event.data.object
%     when 'issuing_dispute.closed'
%         issuing_dispute = event.data.object
%     when 'issuing_dispute.created'
%         issuing_dispute = event.data.object
%     when 'issuing_dispute.funds_reinstated'
%         issuing_dispute = event.data.object
%     when 'issuing_dispute.submitted'
%         issuing_dispute = event.data.object
%     when 'issuing_dispute.updated'
%         issuing_dispute = event.data.object
%     when 'issuing_transaction.created'
%         issuing_transaction = event.data.object
%     when 'issuing_transaction.updated'
%         issuing_transaction = event.data.object
%     when 'mandate.updated'
%         mandate = event.data.object
%     when 'order.created'
%         order = event.data.object
%     when 'payment_link.created'
%         payment_link = event.data.object
%     when 'payment_link.updated'
%         payment_link = event.data.object
%     when 'payment_method.attached'
%         payment_method = event.data.object
%     when 'payment_method.automatically_updated'
%         payment_method = event.data.object
%     when 'payment_method.detached'
%         payment_method = event.data.object
%     when 'payment_method.updated'
%         payment_method = event.data.object
%     when 'payout.canceled'
%         payout = event.data.object
%     when 'payout.created'
%         payout = event.data.object
%     when 'payout.failed'
%         payout = event.data.object
%     when 'payout.paid'
%         payout = event.data.object
%     when 'payout.updated'
%         payout = event.data.object
%     when 'person.created'
%         person = event.data.object
%     when 'person.deleted'
%         person = event.data.object
%     when 'person.updated'
%         person = event.data.object
%     when 'plan.created'
%         plan = event.data.object
%     when 'plan.deleted'
%         plan = event.data.object
%     when 'plan.updated'
%         plan = event.data.object
%     when 'price.created'
%         price = event.data.object
%     when 'price.deleted'
%         price = event.data.object
%     when 'price.updated'
%         price = event.data.object
%     when 'product.created'
%         product = event.data.object
%     when 'product.deleted'
%         product = event.data.object
%     when 'product.updated'
%         product = event.data.object
%     when 'promotion_code.created'
%         promotion_code = event.data.object
%     when 'promotion_code.updated'
%         promotion_code = event.data.object
%     when 'quote.accepted'
%         quote = event.data.object
%     when 'quote.canceled'
%         quote = event.data.object
%     when 'quote.created'
%         quote = event.data.object
%     when 'quote.finalized'
%         quote = event.data.object
%     when 'radar.early_fraud_warning.created'
%         early_fraud_warning = event.data.object
%     when 'radar.early_fraud_warning.updated'
%         early_fraud_warning = event.data.object
%     when 'recipient.created'
%         recipient = event.data.object
%     when 'recipient.deleted'
%         recipient = event.data.object
%     when 'recipient.updated'
%         recipient = event.data.object
%     when 'reporting.report_run.failed'
%         report_run = event.data.object
%     when 'reporting.report_run.succeeded'
%         report_run = event.data.object
%     when 'review.closed'
%         review = event.data.object
%     when 'review.opened'
%         review = event.data.object
%     when 'setup_intent.canceled'
%         setup_intent = event.data.object
%     when 'setup_intent.created'
%         setup_intent = event.data.object
%     when 'setup_intent.requires_action'
%         setup_intent = event.data.object
%     when 'setup_intent.setup_failed'
%         setup_intent = event.data.object
%     when 'setup_intent.succeeded'
%         setup_intent = event.data.object
%     when 'sigma.scheduled_query_run.created'
%         scheduled_query_run = event.data.object
%     when 'sku.created'
%         sku = event.data.object
%     when 'sku.deleted'
%         sku = event.data.object
%     when 'sku.updated'
%         sku = event.data.object
%     when 'source.canceled'
%         source = event.data.object
%     when 'source.chargeable'
%         source = event.data.object
%     when 'source.failed'
%         source = event.data.object
%     when 'source.mandate_notification'
%         source = event.data.object
%     when 'source.refund_attributes_required'
%         source = event.data.object
%     when 'source.transaction.created'
%         transaction = event.data.object
%     when 'source.transaction.updated'
%         transaction = event.data.object
%     when 'subscription_schedule.aborted'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.canceled'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.completed'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.created'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.expiring'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.released'
%         subscription_schedule = event.data.object
%     when 'subscription_schedule.updated'
%         subscription_schedule = event.data.object
%     when 'tax_rate.created'
%         tax_rate = event.data.object
%     when 'tax_rate.updated'
%         tax_rate = event.data.object
%     when 'terminal.reader.action_failed'
%         reader = event.data.object
%     when 'terminal.reader.action_succeeded'
%         reader = event.data.object
%     when 'test_helpers.test_clock.advancing'
%         test_clock = event.data.object
%     when 'test_helpers.test_clock.created'
%         test_clock = event.data.object
%     when 'test_helpers.test_clock.deleted'
%         test_clock = event.data.object
%     when 'test_helpers.test_clock.internal_failure'
%         test_clock = event.data.object
%     when 'test_helpers.test_clock.ready'
%         test_clock = event.data.object
%     when 'topup.canceled'
%         topup = event.data.object
%     when 'topup.created'
%         topup = event.data.object
%     when 'topup.failed'
%         topup = event.data.object
%     when 'topup.reversed'
%         topup = event.data.object
%     when 'topup.succeeded'
%         topup = event.data.object
%     when 'transfer.created'
%         transfer = event.data.object
%     when 'transfer.reversed'
%         transfer = event.data.object
%     when 'transfer.updated'
%         transfer = event.data.object
%     # ... handle other event types
%     else
%         puts "Unhandled event type: #{event.type}"
%     end

%     status 200
% end

