{% extends "admin_base.tpl" %}

{% block title %}{_ Products _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>
            {_ Products _}
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

        {% with m.search.paged[{paysub_products page=q.page pagelen=100}] as result %}
            <table class="table table-striped do_adminLinkedTable" id="payments">
                <thead>
                    <tr>
                        {% block payment_table_head %}
                            <th width="4%">
                                {_ Active _}
                            </th>
                            <th width="4%">
                                {_ PSP _}
                            </th>
                            <th width="15%">
                                {_ Name _}
                            </th>
                            <th width="30%">
                                {_ Prices _}
                            </th>
                            <th width="15%">
                                {_ User group _}
                            </th>
                            <th width="10%">
                                {_ Modified _}
                            </th>
                        {% endblock %}
                    </tr>
                </thead>

                <tbody>
                {% for p in result %}
                    <tr class="{% if not p.is_active %}text-muted{% endif %}" data-product-id="{{ p.id }}">
                        {% block payment_table_row %}
                            <td class="clickable">
                                {% if p.is_active %}
                                    <span class="label label-success">{_ Active _}</span>
                                {% else %}
                                    <span class="label label-default">{_ Inactive _}</span>
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {{ p.psp|escape }}
                            </td>
                            <td class="clickable">
                                {{ p.name|escape }}
                            </td>
                            <td>
                                <table class="table">
                                    {% for price in p.prices %}
                                        <tr>
                                            <td width="60%" class="clickable">
                                                {{ price.name|escape }}<br>
                                                <small>
                                                    {{ price.count }} <span class="text-muted">{_ subscriptions _}</span>
                                                </small>
                                            </td>
                                            <td width="40%" class="clickable">
                                                {{ price.currency|replace:"EUR":"€"|escape }}&nbsp;{{ price.amount|format_price }}
                                                {% if price.billing_scheme == 'tiered' %}
                                                    <span class="text-muted">{_ (tiered pricing) _}</span>
                                                {% endif %}
                                                <br>
                                                <small class="text-muted">
                                                    {% if price.is_recurring %}
                                                        {_ Recurring _} - {{ price.recurring_period|escape}}
                                                    {% endif %}
                                                </small>
                                            </td>
                                        </tr>
                                    {% endfor %}
                                </table>
                            </td>
                            <td class="clickable">
                                {% if p.user_group_id %}
                                    <a href="{% url admin_edit_rsc id=p.user_group_id %}">
                                        {{ p.user_group_id.title }}
                                    </a>
                                {% else %}
                                    <span class="text-muted">
                                        {_ No user group membership. _}
                                    </span>
                                {% endif %}
                            </td>
                            <td class="clickable">
                                {{ p.modified|date:_"Y-m-d" }} <span class="text-muted">{{ p.modified|date:"H:i"}}</span>
                            </td>
                        {% endblock %}
                    </tr>
                {% empty %}
                    <tr>
                        <td colspan="5">
                            {_ No products found. _}
                        </td>
                    </tr>
                {% endfor %}
                </tbody>
            </table>

            {% pager result=result dispatch=`paysub_admin_invoices_overview` qargs hide_single_page %}
        {% endwith %}

        {% wire name="product-info"
                action={dialog_open title=_"Product" template="_dialog_paysub_product_info.tpl"}
        %}

        {% javascript %}
            $('#payments tbody tr').on('click', function() {
                z_event('product-info', { product_id: $(this).attr('data-product-id') });
            });
        {% endjavascript %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
