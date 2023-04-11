% Definitions for payment notifications

-define(PAYSUB_CURRENCY_DEFAULT, <<"EUR">>).

%% Paysub module notification, happens on subscription status changes.
%% Listen to this if you want to do something for new or changed subscriptions.
%% For example creating an user or sending email.
-record(paysub_subscription, {
    action :: new | update | delete,
    status :: binary(),
    customer :: m_paysub:customer(),
    subscription :: m_paysub:subscription(),
    checkout_status :: m_paysub:checkout_status() | undefined
}).

%% Synchronous notification when a checkout is finalized. The status is one
%% of 'open', 'complete' or 'expired'. In the checkout_status can be checked
%% if the checkout is paid, which user, and the args passed to the checkout
%% creation.
-record(paysub_checkout_done, {
    status :: binary(),
    customer :: m_paysub:customer(),
    checkout_status :: m_paysub:checkout_status()
}).
