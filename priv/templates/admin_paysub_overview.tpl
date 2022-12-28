{% extends "admin_base.tpl" %}

{% block title %}{_ Invoices _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>
            {_ Payments &amp; Subscriptions &ndash; Invoices _}
            {% if q.qrsc_id %}
                <a href="{% url paysub_admin_invoices_overview %}" class="btn btn-default btn-xs">{_ Show all _}</a>
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

        {% with m.search.paged[{paysub_invoices rsc_id=q.qrsc_id page=q.page pagelen=20}] as result %}
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
                {% for inv in result %}
                    {#   payment_status is one of: draft, open, paid, uncollectible, or void #}
                    <tr class="{% if inv.payment_status == 'uncollectible' %}text-danger{% elseif inv.payment_status == 'void' %}text-warning{% endif %}" data-invoice-id="{{ inv.id }}">
                        {% block payment_table_row %}
                            <td class="clickable">
                                {{ inv.created|date:_"Y-m-d" }} <span class="text-muted">{{ inv.created|date:"H:i"}}</span>
                            </td>
                            <td class="clickable">
                                {% if inv.payment_status == 'uncollectible' %}
                                    <span class="label label-danger">
                                        {{ inv.payment_status|escape }}
                                    </span>
                                {% elseif inv.payment_status == 'void' %}
                                    <span class="label label-warning">
                                        {{ inv.payment_status|escape }}
                                    </span>
                                {% elseif inv.payment_status == 'paid' %}
                                    <span class="label label-success">
                                        {{ inv.payment_status|escape }}
                                    </span>
                                {% else %}
                                    <span class="label label-default">
                                        {{ inv.payment_status|escape }}
                                    </span>
                                {% endif %}
                            </td>
                            <td style="text-align: right;" class="clickable">
                                {{ inv.currency|replace:"EUR":"€" }}&nbsp;{{ inv.total|format_price }}
                            </td>
                            <td class="clickable">
                                {% for line in inv.items %}
                                    {{ line.description|escape }}
                                    {% if not forloop.last %}<br>{% endif %}
                                {% endfor %}
                            </td>
                            <td class="clickable">
                                {% if inv.rsc_id %}
                                    <a href="{% url admin_edit_rsc id=inv.rsc_id %}">
                                        {% include "_name.tpl" id=inv.rsc_id %}
                                    </a>
                                {% else %}
                                    {{ inv.name|escape }}
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {% if inv.rsc_id %}
                                    <a href="{% url admin_edit_rsc id=inv.rsc_id %}">
                                        {{ inv.email|escape }}
                                    </a>
                                {% else %}
                                    {{ inv.email|escape }}
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {{ inv.psp|escape }}
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

            {% pager result=result dispatch=`paysub_admin_invoices_overview` qargs hide_single_page %}
        {% endwith %}

        {% wire name="invoice-info"
                action={dialog_open title=_"Invoice" template="_dialog_paysub_invoice_info.tpl"}
        %}

        {% javascript %}
            $('#payments tbody tr').on('click', function() {
                z_event('invoice-info', { invoice_id: $(this).attr('data-invoice-id') });
            });
        {% endjavascript %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
