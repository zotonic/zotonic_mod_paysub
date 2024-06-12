{% if m.paysub.is_allowed_paysub %}
{% with m.paysub.rsc[id].subscriptions.list as subs %}
{% with m.paysub.rsc[id].invoices.count as invoice_count %}
{% with m.paysub.rsc[id].payments.count as payment_count %}
{% if subs or invoice_count or payment_count %}
    <div id="rsc-paysub-subs" class="widget do_adminwidget" data-adminwidget="minifiedOnInit:false, minifier:true">
        <div class="widget-header">
            {_ Payments &amp; Subscriptions &ndash; Subscriptions _}
            <div class="widget-header-tools"></div>
        </div>
        <div class="widget-content">
            {% include "_paysub_subscriptions_table.tpl" result=subs %}
            <p>
                <a class="btn btn-default" href="{% url paysub_admin_subscriptions_overview qrsc_id=id %}">{_ View subscriptions _}</a>
                <a class="btn btn-default" href="{% url paysub_admin_invoices_overview qrsc_id=id %}">{_ View invoices _}</a>
                <a class="btn btn-default" href="{% url paysub_admin_payments_overview qrsc_id=id %}">{_ View Payments _}</a>
            </p>
        </div>
    </div>
{% endif %}
{% endwith %}
{% endwith %}
{% endwith %}
{% endif %}
