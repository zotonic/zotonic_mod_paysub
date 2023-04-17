{# Make a button for a link to the PSP customer portal for the given user #}

{% with customer_id|default:m.acl.user as customer_id %}
{% if m.paysub.is_customer_portal.stripe[customer_id] %}
    <a id="{{ #cust }}" class="btn btn-primary" href="#stripe">{{ title|default:_"Manage subscriptions and payments" }}</a>
    {% wire id=#cust
            postback={customer_portal id=customer_id}
            delegate=`paysub_stripe`
    %}
{% endif %}
{% endwith %}
