{% with m.paysub.subscription[q.subscription_id] as p %}
    {% if p %}
        <table class="table">
            <tr>
                <th>{_ Status _}</th>
                <td>
                    {% if p.status == 'unpaid' %}
                        <span class="label label-danger" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% elseif p.status == 'past_due' %}
                        <span class="label label-warning" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% elseif p.status == 'incomplete' %}
                        <span class="label label-info" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% elseif p.status == 'active' %}
                        <span class="label label-success" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% elseif p.status == 'trialing' %}
                        <span class="label label-success" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% else %}
                        <span class="label label-default" style="font-size: 1.5rem">
                            {{ p.status|escape }}
                        </span>
                    {% endif %}
                    <span class="pull-right">{{ p.modified|date:"Y-m-d" }}
                        <span class="text-muted">{{ p.modified|date:"H:i"}}</span>
                    </span>
                </td>
            </tr>
            <tr>
                <th>{_ Start _}</th>
                <td>
                    {% if p.started_at or p.ended_at %}
                        {{ p.started_at|date:_"Y-m-d" }} <span class="text-muted">{{ p.started_at|date:"H:i"}}</span>
                        &mdash;
                        {% if p.ended_at %}
                            {{ p.ended_at|date:_"Y-m-d" }} <span class="text-muted">{{ p.ended_at|date:"H:i"}}</span>
                        {% else %}
                            <span class="text-muted">{_ ongoing _}</span>
                        {% endif %}
                    {% else %}
                        <span class="text-muted">{_ Not started _}</span>
                    {% endif %}
                </td>
            </tr>
            {% if p.period_start or p.period_end %}
                <tr>
                    <th>{_ Period _}</th>
                    <td>
                        {{ p.period_start|date:"Y-m-d" }} <span class="text-muted">{{ p.period_start|date:"H:i"}}</span>
                        &mdash;
                        {{ p.period_end|date:"Y-m-d" }} <span class="text-muted">{{ p.period_end|date:"H:i"}}</span>
                    </td>
                </tr>
            {% endif %}
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
                        {{ p.name|escape }}
                    {% endif %}
                </td>
            </tr>
            {% if p.email|escape|default:p.rsc_id.billing_email|default:p.rsc_id.email as email %}
                <tr>
                    <th>{_ Email _}</th>
                    <td>
                        <a href="mailto:{{ email }}">{{ email }}</a>
                    </td>
                </tr>
            {% endif %}
            <tr>
                <th>{_ Address _}</th>
                <td>
                    {% if p.addres_country %}
                        {% if p.address_street_1 %}{{ p.address_street_1|escape }}<br>{% endif %}
                        {% if p.address_street_2 %}{{ p.address_street_2|escape }}<br>{% endif %}
                        {% if p.address_postcode or p.address_city%}{{ p.address_postcode|escape }}  {{ p.address_city|escape }}<br>{% endif %}
                        {{ m.l10n.country_name[p.address_country]|escape }}
                    {% elseif p.rsc_id.billing_country %}
                        {% if p.rsc_id.billing_street_1 %}{{ p.rsc_id.billing_street_1 }}<br>{% endif %}
                        {% if p.rsc_id.billing_street_2 %}{{ p.rsc_id.billing_street_2 }}<br>{% endif %}
                        {% if p.rsc_id.billing_postcode or p.billing_city%}{{ p.rsc_id.billing_postcode }}  {{ p.rsc_id.billing_city }}<br>{% endif %}
                        {{ m.l10n.country_name[p.rsc_id.billing_country] }}
                    {% else %}
                        {% if p.rsc_id.address_street_1 %}{{ p.rsc_id.address_street_1 }}<br>{% endif %}
                        {% if p.rsc_id.address_street_2 %}{{ p.rsc_id.address_street_2 }}<br>{% endif %}
                        {% if p.rsc_id.address_postcode or p.address_city%}{{ p.rsc_id.address_postcode }}  {{ p.rsc_id.address_city }}<br>{% endif %}
                        {{ m.l10n.country_name[p.rsc_id.address_country] }}
                    {% endif %}
                </td>
            </tr>
            {% if p.phone|escape|default:p.rsc_id.phone|default:p.rsc_id.phone_mobile as phone %}
                <tr>
                    <th>{_ Phone _}</th>
                    <td>
                        <span class="glyphicon glyphicon-phone"></span>
                        <a href="tel:{{ phone }}">{{ phone }}</a>
                    </td>
                </tr>
            {% endif %}
            {% for contact_id in p.rsc_id.o.hasmaincontact %}
                <tr>
                    <th><span class="glyphicon glyphicon-user"></span> {_ Main contact _}</th>
                    <td>
                        <ul class="list-unstyled">
                            <li>
                                <a href="{% url admin_edit_rsc id=contact_id %}">{% include "_name.tpl" id=contact_id %}</a>
                            </li>
                            {% if contact_id.billing_email|default:contact_id.email as email %}
                                <li>
                                    <span class="glyphicon glyphicon-envelope"></span>
                                    <a href="mailto:{{ email }}">{{ email }}</a>
                                </li>
                            {% endif %}
                            {% if contact_id.phone|default:contact_id.phone_mobile as phone %}
                                <li>
                                    <span class="glyphicon glyphicon-phone"></span>
                                    <a href="tel:{{ phone }}">{{ phone }}</a>
                                </li>
                            {% endif %}
                        </ul>
                    </td>
                </tr>

            {% endfor %}
            <tr>
                <th>{_ Items _}</th>
                <td>
                    <table class="table">
                        {% for item in p.items %}
                            <tr>
                                <td>
                                    {{ item.name|escape }}
                                </td>
                                <td>
                                    {{ item.currency|replace:"EUR":"€"|escape }}&nbsp;{{ item.amount|format_price }}<br>
                                    <small class="text-muted">
                                        {% if item.is_recurring %}
                                            {_ Recurring _} - {{ item.recurring_period|escape}}
                                        {% endif %}
                                    </small>
                                </td>
                            </tr>
                        {% endfor %}
                    </table>
                </td>
            </tr>
            <tr>
                <th>{_ PSP _}</th>
                <td>
                    {# TODO: make flexible for PSP #}
                    {% if p.psp == 'stripe' %}
                        <a href="https://stripe.com/"
                           class="btn btn-primary btn-xs" target="payment-psp">
                            <span class="glyphicon glyphicon-new-window"></span>
                            {% trans "view at {psp}" psp= p.psp|capfirst|escape %}
                        </a>
                    {% else %}
                        {{ p.psp|escape }}
                        <span class="text-muted">| {{ p.psp_subscription_id|escape }}</span>
                    {% endif %}
                </td>
            </tr>
        </table>
    {% else %}
        <p class="text-muted">
            {_ Unknown subscription, or no permission to view this subscription. _}
        </p>
    {% endif %}

    <div class="modal-footer">
        {% button class="btn btn-primary" action={dialog_close} text=_"Close" tag="a" %}
    </div>

{% endwith %}
