# zotonic_mod_paysub

Paid memberships and payments for Zotonic, with a bundled Stripe integration.
The module stores customers, products, prices, subscriptions, invoices and
payments locally, and derives membership access from subscription products.

## Setup

1. Enable `mod_paysub`. Its declared dependencies are `mod_authentication` and
   `mod_l10n`. Use `mod_acl_user_groups` for subscription-based user groups.
2. Configure the per-site API key and webhook signing secret listed below.
3. Configure Stripe to send events to the site's `/paysub/stripe/webhook` endpoint.
4. Open **Modules > Payments - Products** in the admin and fetch products and
   prices. Alternatively, call `mod_paysub:sync_products(Context)` from Erlang.
5. Associate each membership product with a user group and configure that group's
   access rules. Add checkout and customer-portal links to the site's templates.

Schema installation creates the payment tables. When the ACL user-group category
exists, it also creates `ug_inactive_member` (members without subscriptions).

## Configuration

These are per-site `m_config` values under `mod_paysub`:

| Key | Default | Purpose |
| --- | --- | --- |
| `stripe_api_key` | Empty | API key used for Stripe requests. |
| `stripe_webhook_secret` | Empty | Signing key for the Stripe webhook endpoint. |
| `is_unpaid_access` | `false` | Include `incomplete_expired` and `unpaid` subscriptions in access-granting states. |
| `is_no_customer_sync` | `false` | Disable copying provider customer billing changes back to Zotonic resource fields. |

A nonempty API key marks Stripe as configured; this does not check credentials or
connectivity. Product synchronization returns `ok` without fetching anything if
Stripe is unconfigured. The `is_no_customer_sync` option affects synchronization
from the provider to the resource; it does not disable outgoing billing updates.

## Webhook

The dispatch name is `paysub_stripe_webhook`. Generate its absolute URL with:

```django
{% url paysub_stripe_webhook absolute_url %}
```

The controller accepts POST requests, verifies the `stripe-signature` header
against the raw body, and requires the timestamp to be within 300 seconds of
server time. No Zotonic user session is required. The timestamp check does not
provide event-ID deduplication.

The implementation handles these events:

| Events | Effect |
| --- | --- |
| `checkout.session.completed` | Complete the checkout locally. |
| `checkout.session.async_payment_succeeded`, `checkout.session.async_payment_failed`, `checkout.session.expired` | Synchronize checkout state. |
| `invoice.created`, `invoice.updated`, `invoice.voided`, `invoice.finalized`, `invoice.paid`, `invoice.payment_failed` | Synchronize the invoice. |
| `invoice.deleted` | Delete the local invoice. |
| `customer.created`, `customer.updated`, `customer.deleted` | Synchronize or delete the local customer. |
| `customer.subscription.created`, `customer.subscription.updated`, `customer.subscription.deleted` | Synchronize subscription state. |
| `price.*`, `product.*` | Schedule price or product synchronization. |
| `payment_intent.*` | Synchronize the payment. |

Unknown events are acknowledged without updating records. Some synchronization
runs asynchronously, so acknowledgement does not mean all local records have
already been refreshed. A handler returning an error produces HTTP 500.

## Membership access

When a user context is initialized, the module adds the user groups associated
with that user's subscription products. It also considers subscriptions of
organizations for which the user is a `hasmaincontact` contact, and those
organizations' `hasusergroup` edges. If none of these yield a group,
`ug_inactive_member` is added.

The access-granting states are:

- Always: `incomplete`, `trialing`, `active`, `past_due`.
- With `is_unpaid_access`: also `incomplete_expired`, `unpaid`.
- `canceled` does not grant subscription access.

Membership checks and group derivation use these states without checking the
subscription end date. Some administrative searches, including `all_active`
and `all_access`, additionally filter on `ended_at`. Product groups supply
membership; their ACL rules determine which site content is accessible.

## Template integration

Start a checkout using a provider price ID that has been synchronized locally:

```django
{% include "_paysub_checkout_link.tpl"
    psp="stripe"
    price="price_example"
    quantity=1
%}
```

Replace `price_example` with the Stripe price ID. The adapter selects subscription
or one-time payment mode from the price's recurring status. The checkout flow
returns through the `paysub_psp_done` or `paysub_psp_cancel` dispatch, with a
checkout number. The module supplies completion and welcome templates that the
site can override.

Show the current user's customer-portal link:

```django
{% include "_paysub_customer_link.tpl" %}
```

Pass `customer_id=id` to target another resource. The partial only displays a
link when portal access is available. Portal access is restricted to the user
or a main contact of the resource.

Check membership or display subscriptions:

```django
{% if m.paysub.is_subscriber %}
    <p>{_ You have a subscription. _}</p>
{% endif %}

{% for sub in m.paysub.rsc[id].subscriptions.list %}
    <p>{{ sub.status|escape }}</p>
{% endfor %}
```

Other model paths include `rsc[id].invoices.list`, `rsc[id].payments.list`, their
corresponding `.count` paths, and `checkout.status[checkout_nr]`. Checkout status
is looked up by checkout number without an ownership check; treat that number
as a private flow identifier.

## Administration and permissions

The admin Modules menu provides Payments dashboard, invoice, subscription,
product and payment views. Management requires administrator status or `use`
permission for `mod_paysub`. Individual actions can additionally require resource
edit permission.

Resource subscription, invoice and payment list/count paths allow Paysub
administrators, the resource's own user, or its main contacts. Direct Erlang
lookup and synchronization helpers do not necessarily repeat these checks;
authorize callers before using them.

Named searches exposed through `m.search` are `paysub_subscriptions`,
`paysub_invoices`, `paysub_payments` and `paysub_products`. These searches require
Paysub management permission. The resource query terms `is_paysub_subscriber`
and `paysub_subscription_status` support membership filtering.

The module also provides subscription spreadsheet export, survey payment-status
columns, resource-merge handling, and subscription moves between resources.
Resource pivot updates schedule billing changes toward Stripe; customer events
copy changed billing fields back unless `is_no_customer_sync` is enabled.

## Source reference

- [Module setup and observers](src/mod_paysub.erl)
- [Payment and subscription model](src/models/m_paysub.erl)
- [Search model and query terms](src/models/m_paysub_search.erl)
- [Webhook authentication and event handling](src/controllers/controller_paysub_stripe_webhook.erl)
- [Stripe checkout and synchronization adapter](src/support/paysub_stripe.erl)
