{% extends "admin_base.tpl" %}

{% block title %}{_ Subscriptions _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>
            {_ Payments &amp; Subscriptions &ndash; Subscriptions _}
            {% if q.qrsc_id %}
                <a href="{% url paysub_admin_subscriptions_overview %}" class="btn btn-default btn-xs">{_ Show all _}</a>
            {% endif %}
        </h2>
    </div>
    {% if m.acl.is_admin or m.acl.is_allowed.use.mod_paysub %}
        {#
            <div class="well z-button-row">
                <a name="content-pager"></a>

                {% button
                    class="btn btn-primary"
                    text=_"Export"
                    action={redirect dispatch="export_payments_csv"}
                %}

                {% button
                    class="btn btn-primary"
                    text=_"Sync new &amp; pending"
                    postback={sync_pending}
                    delegate=`mod_payment`
                %}
            </div>
        #}

        {% with m.search.paged[{paysub_subscriptions
                    rsc_id=q.qrsc_id
                    price_id=q.qprice_id
                    product_id=q.qproduct_id
                    page=q.page
                    pagelen=20
            }] as result %}
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
                            <th width="20%">
                                {_ Items _}
                            </th>
                            <th width="15%">
                                {_ Name _}
                            </th>
                            <th width="15%">
                                {_ Email _}
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
                            {% with sub.rsc_id.o.hasmaincontact[1] as contact_id %}
                                <td class="clickable">
                                    {% if sub.rsc_id %}
                                        <a href="{% url admin_edit_rsc id=sub.rsc_id %}">
                                            {% include "_name.tpl" id=sub.rsc_id %}
                                        </a>
                                        {% if sub.rsc_id.o.hasmaincontact[1] as contact %}
                                            <br><span class="glyphicon glyphicon-user"></span>
                                            <a href="{% url admin_edit_rsc id=contact_id %}">
                                                {% include "_name.tpl" id=contact_id %}
                                            </a>
                                        {% endif %}
                                    {% else %}
                                        {{ sub.name|escape }}
                                    {% endif %}
                                </td>
                                <td class="clickable">
                                    {% if sub.rsc_id %}
                                        {% if sub.email|escape|default:sub.rsc_id.billing_email|default:sub.rsc_id.email as email %}
                                            <a href="mailto:{{ email }}">
                                                {{ email }}
                                            </a><br>
                                        {% endif %}
                                        {% if sub.rsc_id.o.hasmaincontact[1] as contact %}
                                            <span class="glyphicon glyphicon-user"></span>
                                            <a href="mailto:{{ contact_id.email }}">
                                                {{ contact_id.email }}
                                            </a>
                                        {% endif %}
                                    {% else %}
                                        <a href="mailto:{{ sub.email|escape }}">
                                            {{ sub.email|escape }}
                                        </a>
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

            {% pager result=result dispatch=`paysub_admin_subscriptions_overview` qargs hide_single_page %}
        {% endwith %}

        {% wire name="subscription-info"
                action={dialog_open title=_"Subscription" template="_dialog_paysub_subscription_info.tpl"}
        %}

        {% javascript %}
            $('#payments tbody tr').on('click', function() {
                z_event('subscription-info', { subscription_id: $(this).attr('data-subscription-id') });
            });
        {% endjavascript %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
