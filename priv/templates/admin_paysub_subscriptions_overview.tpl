{% extends "admin_base.tpl" %}

{% block title %}{_ Subscriptions _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <a href="{% url paysub_export_subscriptions %}" rel="download" class="btn btn-default pull-right">
            {_ Download CSV _}
        </a>

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
            {% include "_paysub_subscriptions_table.tpl" result=result %}
            {% pager result=result dispatch=`paysub_admin_subscriptions_overview` qargs hide_single_page %}
        {% endwith %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
