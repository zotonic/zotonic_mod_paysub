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

            {% if id.s.hasmaincontact[1] as org_id %}
                <p class="pull-right text-right">
                    {% if m.paysub.has_moveable_maincontact_subs[id] and org_id.is_editable and id.is_editable %}
                        <button type="button" class="btn btn-default pull-right" id="move-sub-org" style="margin-bottom: 4px;">
                            {% trans "Move subscriptions to {org}" org=org_id.title %}
                        </button>

                        {% wire id="move-sub-org"
                                action={confirm
                                    title=_"Move subscriptions from main contact to organization"
                                    text=[
                                        _"Do you want to move subscriptions to:", " <b>",
                                        org_id.title,
                                        "</b>?",
                                        "<br><br>",
                                        _"Only subscriptions for products which are configured to use a main contact are moved.",
                                        "<br><br>",
                                        _"This cannot be undone."
                                    ]
                                    ok=_"Move subscriptions"
                                    postback={move_subscriptions
                                        from_id=id
                                        to_id=org_id
                                        is_only_maincontact
                                        on_success={reload}
                                    }
                                    delegate=`mod_paysub`
                                }
                        %}

                        <br>
                    {% endif %}

                    <a href="{% url admin_edit_rsc id=org_id %}">
                        {_ Go to _} {{ org_id.title }} &gt;
                    </a>
                </p>
            {% endif %}

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
