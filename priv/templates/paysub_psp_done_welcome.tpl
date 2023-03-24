{% extends "page.tpl" %}

{% block title %}{_ Welcome _} - {{ m.site.title }}{% endblock %}

{% block body %}
    {{ id.payment_done_html|show_media }}

    {% if m.acl.user %}
        <p>
            <a class="btn btn-primary" href="{{ m.acl.user.page_url }}">{_ Continue to your profile page _}</a>
        </p>
    {% endif %}
{% endblock %}
