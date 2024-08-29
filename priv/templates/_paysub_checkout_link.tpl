{# Make a button for a link to a checkout #}

{% if not price %}
    <p class="text-danger">No 'price' given for checkout link template.</p>
{% elseif psp == 'stripe' %}
    <a id="{{ #sub }}" class="btn btn-primary" href="#subscribe">{{ title|default:_"Subscribe" }}</a>
    {% wire id=#sub
            postback={checkout price=price quantity=quantity currency=currency}
            delegate=`paysub_stripe`
    %}
{% else %}
    <p class="text-danger">No 'psp' given for checkout link template.</p>
{% endif %}
