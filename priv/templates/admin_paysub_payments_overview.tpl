{% extends "admin_base.tpl" %}

{% block title %}{_ Payments _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>
            {_ Payments _}
            {% if q.qrsc_id %}
                <a href="{% url paysub_admin_payments_overview %}" class="btn btn-default btn-xs">{_ Show all _}</a>
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

        {% with m.search.paged[{paysub_payments rsc_id=q.qrsc_id page=q.page pagelen=100}] as result %}
            <table class="table table-striped do_adminLinkedTable" id="payments">
                <thead>
                    <tr>
                        {% block payment_table_head %}
                            <th width="10%">
                                {_ Date _}
                            </th>
                            <th width="6%">
                                {_ Status _}
                            </th>
                            <th width="7%" style="text-align: right;">
                                {_ Amount _}
                            </th>
                            <th width="7%" style="text-align: right;">
                                {_ Received _}
                            </th>
                            <th width="20%">
                                {_ Description _}
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
                {% for p in result %}
                    {# status is one of: requires_payment_method, requires_confirmation, requires_action,
                     #                   processing, requires_capture, canceled, or succeeded
                     #}
                    <tr data-payment-id="{{ p.id }}">
                        {% block payment_table_row %}
                            <td class="clickable">
                                {{ p.created|date:_"Y-m-d" }} <span class="text-muted">{{ p.created|date:"H:i"}}</span>
                            </td>
                            <td class="clickable">
                                {% if p.payment_status == 'canceled' %}
                                    <span class="label label-warning">
                                        {{ p.status|escape }}
                                    </span>
                                {% elseif p.status == 'succeeded' %}
                                    <span class="label label-success">
                                        {{ p.status|escape }}
                                    </span>
                                {% else %}
                                    <span class="label label-default">
                                        {{ p.status|escape }}
                                    </span>
                                {% endif %}
                            </td>
                            <td style="text-align: right;" class="clickable">
                                {{ p.currency|replace:"EUR":"€" }}&nbsp;{{ p.amount|format_price }}
                            </td>
                            <td style="text-align: right;" class="clickable">
                                {% if p.amount_received %}
                                    {{ p.currency|replace:"EUR":"€" }}&nbsp;{{ p.amount_received|format_price }}
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {{ p.description|escape }}
                            </td>
                            <td class="clickable">
                                {{ p.customer_name|default:p.name|escape }}
                            </td>
                            <td class="clickable">
                                {{ p.customer_email|default:p.email|escape }}
                            </td>
                            <td class="clickable">
                                {{ p.psp|escape }}
                            </td>
                        {% endblock %}
                    </tr>
                {% empty %}
                    <tr>
                        <td colspan="5">
                            {_ No payments found. _}
                        </td>
                    </tr>
                {% endfor %}
                </tbody>
            </table>

            {% pager result=result dispatch=`paysub_admin_payments_overview` qargs hide_single_page %}
        {% endwith %}

        {% wire name="payment-info"
                action={dialog_open title=_"Payment" template="_dialog_paysub_payment_info.tpl"}
        %}

        {% javascript %}
            $('#payments tbody tr').on('click', function() {
                z_event('payment-info', { payment_id: $(this).attr('data-payment-id') });
            });
        {% endjavascript %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
