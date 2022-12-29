{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}
{_ Payments &amp; subscriptions _}
<div class="widget-header-tools"></div>
{% endblock %}

{% block widget_show_minimized %}false{% endblock %}
{% block widget_id %}sidebar-paysub{% endblock %}

{% block widget_content %}
    <div class="form-group">
        <a class="btn btn-default" href="{% url paysub_admin_invoices_overview qrsc_id=id %}">{_ View invoices _}</a>
        <a class="btn btn-default" href="{% url paysub_admin_subscriptions_overview qrsc_id=id %}">{_ View subscriptions _}</a>
    </div>
{% endblock %}

