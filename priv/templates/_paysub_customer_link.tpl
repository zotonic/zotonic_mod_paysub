{# Make a button for a link to the PSP customer portal for the given user #}

{% if m.paysub.is_customer_portal.stripe %}
    <a id="{{ #cust }}" class="btn btn-primary" href="#stripe">{{ title|default:_"Manage subscriptions and payments" }}</a>
    {% wire id=#cust
            postback={customer_portal id=m.acl.user}
            delegate=`paysub_stripe`
    %}
{% endif %}
