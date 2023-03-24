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
    checkout_status :: m_paysub:checkout_status()
}).
