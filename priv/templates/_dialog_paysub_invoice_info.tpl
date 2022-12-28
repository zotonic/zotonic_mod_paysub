{% with m.paysub.invoice[q.invoice_id] as p %}
    {% if p %}
        <table class="table">
            <tr>
                <th>{_ Status _}</th>
                <td>
                    {% if p.payment_status == 'uncollectible' %}
                        <span class="label label-danger" style="font-size: 1.5rem">
                            {{ p.payment_status|escape }}
                        </span>
                    {% elseif p.payment_status == 'void' %}
                        <span class="label label-warning" style="font-size: 1.5rem">
                            {{ p.payment_status|escape }}
                        </span>
                    {% elseif p.payment_status == 'paid' %}
                        <span class="label label-success" style="font-size: 1.5rem">
                            {{ p.payment_status|escape }}
                        </span>
                    {% else %}
                        <span class="label label-default" style="font-size: 1.5rem">
                            {{ p.payment_status|escape }}
                        </span>
                    {% endif %}
                    <span class="pull-right">{{ p.created|date:"Y-m-d" }}
                        <span class="text-muted">{{ p.created|date:"H:i"}}</span>
                    </span>
                </td>
            </tr>
            <tr>
                <th>{_ Amount _}</th>
                <td>
                    <h3 style="margin-top:0">{{ p.currency|replace:"EUR":"€"|escape }} {{ p.total|format_price }}</h3>
                    <table class="table">
                        <tr>
                            <th>{_ Amount due _}</th>
                            <th>{_ Amount paid _}</th>
                            <th>{_ Amount remaining _}</th>
                        </tr>
                        <tr>
                            <td>{{ p.amount_due|format_price }}</td>
                            <td>{{ p.amount_paid|format_price }}</td>
                            <td>{{ p.amount_remaining|format_price }}</td>
                        </tr>
                    </table>
                </td>
            </tr>
            <tr>
                <th>{_ Name _}</th>
                <td>
                    {% if p.rsc_id %}
                        <a href="{% url admin_edit_rsc id=p.rsc_id %}">{% include "_name.tpl" id=p.rsc_id %}</a>

                        <span class="pull-right">
                            <a href="{% url paysub_admin_invoices_overview qrsc_id=p.rsc_id %}">{_ Show all invoices _} &gt;</a>
                        </span>
                    {% else %}
                        {{ p.name|escape }}
                    {% endif %}
                </td>
            </tr>
            <tr>
                <th>{_ Email _}</th>
                <td>
                    {% if p.email|escape|default:p.rsc_id.email as email %}
                        <a href="mailto:{{ email }}">{{ email }}</a>
                    {% endif %}
                </td>
            </tr>
            <tr>
                <th>{_ Address _}</th>
                <td>
                    {% if p.address_street_1 %}{{ p.address_street_1|escape }}<br>{% endif %}
                    {% if p.address_street_2 %}{{ p.address_street_2|escape }}<br>{% endif %}
                    {% if p.address_postcode or p.address_city%}{{ p.address_postcode|escape }}  {{ p.address_city|escape }}<br>{% endif %}
                    {{ m.l10n.country_name[p.address_country]|escape }}
                </td>
            </tr>
            {% if p.phone %}
                <tr>
                    <th>{_ Phone _}</th>
                    <td>
                        {{ p.phone|escape }}
                    </td>
                </tr>
            {% endif %}
            <tr>
                <th>{_ Items _}</th>
                <td>
                    <table class="table">
                        {% for item in p.items %}
                            <tr>
                                <td>
                                    {{ item.description|escape }}
                                </td>
                                <td>
                                    {{ item.currency|replace:"EUR":"€"|escape }}&nbsp;{{ item.amount|format_price }}
                                    {% if item.is_prorated %}
                                        <br><span class="label label-default">{_ Prorated _}</span>
                                    {% endif %}
                                </td>
                            </tr>
                        {% endfor %}
                    </table>
                </td>
            </tr>
            {% if p.period_start or p.period_end %}
                <tr>
                    <th>{_ Period _}</th>
                    <td>
                        {{ p.period_start|date:"Y-m-d" }} &ndash; {{ p.period_end|date:"Y-m-d" }}
                    </td>
                </tr>
            {% endif %}
            <tr>
                <th>{_ Payment Service Provider _}</th>
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
                        <span class="text-muted">| {{ p.psp_invoice_id|escape }}</span>
                    {% endif %}
                </td>
            </tr>
        </table>
    {% else %}
        <p class="text-muted">
            {_ Unknown invoice, or no permission to view this invoice. _}
        </p>
    {% endif %}

    <div class="modal-footer">
        {% button class="btn btn-primary" action={dialog_close} text=_"Close" tag="a" %}
    </div>

{% endwith %}
