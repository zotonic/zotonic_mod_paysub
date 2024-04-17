{% extends "admin_base.tpl" %}

{% block title %}{_ Subscriptions dashboard _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <h2>
            {_ Subscriptions dashboard _}
            {% if q.qrsc_id %}
                <a href="{% url paysub_admin_dashboard %}" class="btn btn-default btn-xs">{_ Show all _}</a>
            {% endif %}
        </h2>
        <p>{_ Quick overview of current subscriptions per user group and totals over the last years. _}</p>
    </div>

    <h3>{_ Current subscriptions _}</h3>
    <p>
        {_ An overview of user groups and the number of subscriptions per user group. _}<br>
        {_ Click on a number to see those subscriptions. _}
    </p>

    <table class="table table-striped table-compact" id="paysub-dashboard" style="width: auto">
        <thead>
            <tr>
                <th>
                    {_ User group _}
                </th>
                <th>
                    {_ Active _}
                </th>
                <th>
                    {_ Incomplete _}
                </th>
                <th>
                    {_ Past due _}
                </th>
                <th>
                    {_ Trialing _}
                </th>
                <th>
                    {_ Unpaid _}
                </th>
                <th>
                    {_ Incomplete<br>expired _}
                </th>
                <th style="border-left: 1px solid black">
                    {_ TOTAL _}
                </th>
            </tr>
        </thead>
        <tbody>
        {% with [ 'active', 'incomplete', 'past_due', 'trialing', 'unpaid', 'incomplete_expired' ] as states %}
            {% with m.paysub.overview_by.user_group as r %}
                {% for id in r.user_groups %}
                <tr>
                    <td>
                        {{ id.title }}
                    </td>
                    {% for s in states %}
                        <td class="do_clickable">
                            <a href="{% url paysub_admin_subscriptions_overview qstatus=s quser_group_id=id %}">
                                {{ r.status[id][s]|default:0 }}
                            </a>
                        </td>
                    {% endfor %}
                    <td style="border-left: 1px solid black">
                        <a href="{% url paysub_admin_subscriptions_overview quser_group_id=id qstatus='uncanceled' %}">
                            {{ r.status[id].total|default:0 }}
                        </a>
                    </td>
                </tr>
                {% endfor %}

                <tr>
                    <td style="border-top: 1px solid black">
                        <b>{_ TOTAL _}</b>
                    </td>
                    {% for s in states %}
                        <td style="border-top: 1px solid black">
                            <a href="{% url paysub_admin_subscriptions_overview qstatus=s %}">
                                <b>{{ r.total[s]|default:0 }}</b>
                            </a>
                        </td>
                    {% endfor %}
                    <td style="border-top: 1px solid black; border-left: 1px solid black">
                        <a href="{% url paysub_admin_subscriptions_overview qstatus='uncanceled' %}">
                            <b>{{ r.total.total|default:0 }}</b>
                        </a>
                    </td>
                </tr>

            {% endwith %}
        {% endwith %}
        </tbody>
    </table>
{% endblock %}
