{% with m.paysub.payment[q.payment_id] as p %}
    {% if p %}
        <table class="table">
            <tr>
                <th>{_ Status _}</th>
                <td>
                    {% if p.payment_status == 'canceled' %}
                        <span class="label label-warning" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% elseif p.status == 'succeeded' %}
                        <span class="label label-success" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% else %}
                        <span class="label label-default" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% endif %}
                </td>
            </tr>
            <tr>
                <th>{_ Created _}</th>
                <td>
                    {{ p.created|date:_"Y-m-d" }} <span class="text-muted">{{ p.created|date:"H:i"}}</span>
                </td>
            </tr>
            <tr>
                <th>{_ Amount _}</th>
                <td>
                    {{ p.currency|replace:"EUR":"€" }}&nbsp;{{ p.amount|format_price }}
                </td>
            </tr>
            <tr>
                <th>{_ Amount received _}</th>
                <td>
                    {% if p.amount_received %}
                        {{ p.currency|replace:"EUR":"€" }}&nbsp;{{ p.amount_received|format_price }}
                    {% endif %}
                </td>
            </tr>
            <tr>
                <th>{_ Name _}</th>
                <td>
                    {% if p.rsc_id %}
                        <div class="pull-right" style="padding-left:10px;">
                            <a href="{% url paysub_admin_invoices_overview qrsc_id=p.rsc_id %}">{_ Show all invoices _} &gt;</a><br>
                            <a href="{% url paysub_admin_subscriptions_overview qrsc_id=p.rsc_id %}">{_ Show all subscriptions _} &gt;</a><br>
                            <a href="{% url paysub_admin_payments_overview qrsc_id=p.rsc_id %}">{_ Show all payments _} &gt;</a>
                        </div>
                        <a href="{% url admin_edit_rsc id=p.rsc_id %}">{% include "_name.tpl" id=p.rsc_id %}</a>
                    {% else %}
                        {{ p.name|default:p.customer_name|escape }}
                    {% endif %}
                </td>
            </tr>
            {% if p.email
                    |default:p.customer_email
                    |escape
                    |default:p.rsc_id.billing_email
                    |default:p.rsc_id.email as email
            %}
                <tr>
                    <th>{_ Email _}</th>
                    <td>
                        <a href="mailto:{{ email }}">{{ email }}</a>
                    </td>
                </tr>
            {% endif %}
            <tr>
                <th>{_ PSP _}</th>
                <td>
                    {# TODO: make flexible for PSP #}
                    {% if p.psp == 'stripe' %}
                        <a href="https://stripe.com/"
                           class="btn btn-primary btn-xs" target="payment-psp">
                            <span class="glyphicon glyphicon-new-window"></span>
                            {% trans "view at {psp}" psp=p.psp|capfirst|escape %}
                        </a>
                    {% else %}
                        {{ p.psp|escape }}
                        <span class="text-muted">| {{ p.psp_payment_id|escape }}</span>
                    {% endif %}
                </td>
            </tr>
        </table>

        <div class="modal-footer">
            {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
        </div>
    {% else %}
        <p class="text-muted">
            {_ Unknown payment, or no permission to view this payment. _}
        </p>
        <div class="modal-footer">
            {% button class="btn btn-primary" action={dialog_close} text=_"Close" tag="a" %}
        </div>
    {% endif %}

{% endwith %}
