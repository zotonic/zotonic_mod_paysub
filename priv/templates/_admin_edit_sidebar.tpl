{% if id.is_a.person
      or m.paysub.rsc[id].invoices.count
      or m.paysub.rsc[id].subscriptions.count
%}
    {% include "_admin_edit_sidebar_paysub.tpl" %}
{% endif %}