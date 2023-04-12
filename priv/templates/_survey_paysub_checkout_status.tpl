{% if not checkout %}
    {% if format == 'html' %}
        <span class="text-muted">&mdash;</span>
    {% else %}
        —
    {% endif %}
{% elseif format == 'html' %}
    {% if checkout.status == 'open' %}
        <span class="label label-default">{_ Open _}</span>
    {% elseif checkout.status == 'complete' %}
        {% if checkout.payment_status == 'paid' %}
            <span class="label label-success">{_ Paid _}</span>
        {% elseif checkout.payment_status == 'no_payment_required' %}
            <span class="label label-success">{_ No payment required _}</span>
        {% elseif checkout.payment_status == 'unpaid' %}
            <span class="label label-warning">{_ Not yet received _}</span>
        {% endif %}
    {% elseif checkout.status == 'expired' %}
        <span class="label label-danger">{_ Expired _}</span>
    {% endif %}

    {% if checkout.psp == 'stripe' and checkout.psp_payment_id %}
        <a href="https://dashboard.stripe.com/payments/{{ checkout.psp_payment_id|urlencode }}"
           class="btn btn-default btn-xs" target="payment-psp">
            <span class="glyphicon glyphicon-new-window"></span>
            {% trans "view at {psp}" psp=checkout.psp|capfirst|escape %}
        </a>
    {% endif %}
{% else %}
    {% if checkout.status == 'open' %}
        {_ Open _}
    {% elseif checkout.status == 'complete' %}
        {% if checkout.payment_status == 'paid' %}
            {_ Paid _}
        {% elseif checkout.payment_status == 'no_payment_required' %}
            {_ No payment required _}
        {% elseif checkout.payment_status == 'unpaid' %}
            {_ Not yet received _}
        {% endif %}
    {% elseif checkout.status == 'expired' %}
        {_ Expired _}
    {% endif %}
{% endif %}
