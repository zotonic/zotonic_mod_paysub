% Definitions for payment notifications

-define(PAYSUB_CURRENCY_DEFAULT, <<"EUR">>).

%% Paysub module notification, happens on subscription status changes.
%% Listen to this if you want to do something for new or changed subscriptions.
%% For example creating an user or sending email.
%% Called with 'first' inside the transaction updating the paysub information.
%% Only called if the subscription is set to 'is_provisioned', this to prevent
%% race conditions during checkout flows.
%% Return 'ok' if all done.
-record(paysub_subscription, {
    action :: new | update | delete,
    status :: binary(),
    customer :: m_paysub:customer(),
    subscription :: m_paysub:subscription(),
    checkout_status :: m_paysub:checkout_status() | undefined
}).

%% Paysub module notification, happens after a subscription updates.
%% It is called for the resource id the subscription is attached to.
%% If the subscription was not attached to a resource then this
%% notification is not called.
%% Called for provisioned and non provisioned subscriptions.
%% Also called if subscriptions are updated due to resource merges.
%% Called async with 'notify'.
-record(paysub_subscription_done, {
    rsc_id :: m_rsc:resource_id()
}).

%% Paysub module notification, happens after a customer updates because
%% of a change at the PSP. Not called for customer updates caused directly
%% by us. Note that indirectly this notification will be triggered
%% if the customer is changed at the PSP by us and then synced back to us.
%% The PaySub module will update the resource, unless another module handled
%% the event, or the config mod_paysub.is_no_customer_sync is set.
%% Called synchronous with 'first'.
-record(paysub_customer, {
    action :: new | update | delete,
    customer :: m_paysub:customer()
}).

%% Synchronous notification when a checkout is finalized. The status is one
%% of 'open', 'complete' or 'expired'. In the checkout_status can be checked
%% if the checkout is paid, which user, and the args passed to the checkout
%% creation.
%% Called with 'notify_sync'.
-record(paysub_checkout_done, {
    status :: binary(),
    customer :: m_paysub:customer(),
    checkout_status :: m_paysub:checkout_status()
}).

%% Generic event if something changes with products or prices. Allows other modules
%% to resync information, if needed.
%% Called with 'notify_sync'.
-record(paysub_event, {
    what :: products | prices
}).
