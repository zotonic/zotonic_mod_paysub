{% if m.paysub.rsc[id].invoices.count or id.is_a.person %}
    {% include "_admin_edit_sidebar_paysub.tpl" %}
{% endif %}