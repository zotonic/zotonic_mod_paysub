% Definitions for payment notifications

-define(PAYSUB_CURRENCY_DEFAULT, <<"EUR">>).

%% Paysub module notification, happens on subscription status changes.
%% Listen to this if you want to do something for new or changed subscriptions.
%% For example creating an user or sending email.
%% Called with 'first' inside the transaction updating the paysub information.
%% Return 'ok' if all done.
-record(paysub_subscription, {
    action :: new | update | delete,
    status :: binary(),
    customer :: m_paysub:customer(),
    subscription :: m_paysub:subscription(),
    checkout_status :: m_paysub:checkout_status() | undefined
}).

%% Paysub module notification, happens after subscription updates.
%% It is called for the resource id the subscription is attached to.
%% If the subscription was not attached to a resource then this
%% notification is not called.
%% Also called if subscriptions are updated due to resource merges.
%% Called async with 'notify'.
-record(paysub_subscription_done, {
    rsc_id :: m_rsc:resource_id()
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
