<table class="table table-striped do_adminLinkedTable" id="payments">
    <thead>
        <tr>
            {% block payment_table_head %}
                <th width="10%">
                    {_ Start date _}
                </th>
                <th width="10%">
                    {_ End date _}
                </th>
                <th width="6%">
                    {_ Status _}
                </th>
                <th width="15%">
                    {_ Items _}
                </th>
                <th width="15%">
                    {_ Name _}
                </th>
                <th width="15%">
                    <span class="glyphicon glyphicon-user"></span> {_ Contact _}
                </th>
                <th width="5%">
                    {_ PSP _}
                </th>
            {% endblock %}
        </tr>
    </thead>

    <tbody>
    {% for sub in result %}
        {#
            <tr>
                <td colspan="10">
                    {% print sub %}
                </td>
            </tr>
        #}
        {# subscription status is one of: incomplete, incomplete_expired, trialing, active, past_due, canceled, or unpaid #}
        <tr class="{% if sub.status == 'unpaid' %}text-danger{% elseif sub.status == 'past_due' %}text-warning{% endif %}" data-subscription-id="{{ sub.id }}">
            {% block payment_table_row %}
                <td class="clickable">
                    {{ sub.started_at|date:_"Y-m-d" }} <span class="text-muted">{{ sub.started_at|date:"H:i"}}</span>
                </td>
                <td class="clickable">
                    {{ sub.ended_at|date:_"Y-m-d" }} <span class="text-muted">{{ sub.ended_at|date:"H:i"}}</span>
                </td>
                <td class="clickable">
                    {% if sub.status == 'unpaid' %}
                        <span class="label label-danger">
                            {{ sub.status|escape }}
                        </span>
                    {% elseif sub.status == 'past_due' %}
                        <span class="label label-warning">
                            {{ sub.status|escape }}
                        </span>
                    {% elseif sub.status == 'incomplete' %}
                        <span class="label label-info">
                            {{ sub.status|escape }}
                        </span>
                    {% elseif sub.status == 'active' %}
                        <span class="label label-success">
                            {{ sub.status|escape }}
                        </span>
                    {% elseif sub.status == 'trialing' %}
                        <span class="label label-success">
                            {{ sub.status|escape }}
                        </span>
                    {% else %}
                        <span class="label label-default">
                            {{ sub.status|escape }}
                        </span>
                    {% endif %}
                </td>
                <td class="clickable">
                    <ul class="list-unstyled">
                        {% for line in sub.items %}
                            <li>
                            {{ line.name|escape }}
                            ({{ line.currency|replace:"EUR":"€" }}&nbsp;{{ line.amount|format_price }})
                            <br>
                            <small class="text-muted">
                                {% if line.is_recurring %}
                                    {_ Recurring _} - {{ line.recurring_period|escape}}
                                {% endif %}
                            </small>
                        {% endfor %}
                    </ul>
                </td>
                <td class="clickable">
                    {% if sub.rsc_id %}
                        <a href="{% url admin_edit_rsc id=sub.rsc_id %}">
                            {% include "_name.tpl" id=sub.rsc_id %}
                        </a><br>
                        {% if sub.email|escape|default:sub.rsc_id.billing_email|default:sub.rsc_id.email as email %}
                            <a href="mailto:{{ email }}">
                                {{ email }}
                            </a><br>
                        {% endif %}
                        {% if sub.rsc_id.phone|default:sub.rsc_id.phone_mobile as phone %}
                            <a href="tel:{{ phone }}">
                                {{ phone }}
                            </a><br>
                        {% endif %}
                        {% if sub.rsc_id.billing_country|default:sub.rsc_id.address_country as country %}
                            {{ m.l10n.country_name[country] }}<br>
                        {% endif %}
                    {% else %}
                        {{ sub.name|escape }}<br>
                        {% if sub.email %}
                            <a href="mailto:{{ sub.email|escape }}">
                                {{ sub.email|escape }}
                            </a><br>
                        {% endif %}
                        {{ m.l10n.country_name[sub.country] }}<br>
                    {% endif %}
                </td>
                {% with sub.rsc_id.o.hasmaincontact[1] as contact_id %}
                    <td class="clickable">
                        {% if contact_id %}
                            <span class="glyphicon glyphicon-user"></span>
                            <a href="{% url admin_edit_rsc id=contact_id %}">
                                {% include "_name.tpl" id=contact_id %}
                            </a><br>
                            {% if contact_id.billing_email|default:contact_id.email as email %}
                                <a href="mailto:{{ email }}">
                                    {{ email }}
                                </a><br>
                            {% endif %}
                            {% if contact_id.phone|default:contact_id.phone_mobile as phone %}
                                <a href="tel:{{ phone }}">
                                    {{ phone }}
                                </a><br>
                            {% endif %}
                        {% endif %}
                        {% if contact_id.billing_country|default:contact_id.address_country as country %}
                            {{ m.l10n.country_name[country] }}
                        {% endif %}
                    </td>
                {% endwith %}
                <td class="clickable">
                    {{ sub.psp|escape }}
                </td>
            {% endblock %}
        </tr>
    {% empty %}
        <tr>
            <td colspan="5">
                {_ No subscriptions found. _}
            </td>
        </tr>
    {% endfor %}
    </tbody>
</table>

{% wire name="subscription-info"
        action={dialog_open title=_"Subscription" template="_dialog_paysub_subscription_info.tpl"}
%}

{% javascript %}
    $('#payments tbody tr').on('click', function() {
        z_event('subscription-info', { subscription_id: $(this).attr('data-subscription-id') });
    });
{% endjavascript %}
