{% extends "admin_base.tpl" %}

{% block title %}{_ Subscriptions _}{% endblock %}

{% block content %}
    <div class="admin-header">
        <a href="{% url paysub_export_subscriptions qargs %}" rel="download" class="btn btn-default pull-right">
            {_ Download XLSX _}
        </a>

        <h2>
            {_ Subscriptions _}
        </h2>
    </div>

    {% if m.acl.is_admin or m.acl.is_allowed.use.mod_paysub %}
        <div class="well">
            {% if m.rsc[q.qrsc_id].id as id %}
                <p>
                    {_ Showing only for _}: <b>{% include "_name.tpl" id=id %}</b>
                    <a href="{% url paysub_admin_subscriptions_overview %}" class="btn btn-default btn-xs">{_ Show all _}</a>
                </p>
            {% endif %}

            <form id="filter-form" method="GET" action="" class="do_forminit">
                <div class="row">
                    <div class="col-sm-2 col-xs-6">
                        <label class="control-label">{_ Status _}</label>
                        <select class="form-control" name="qstatus">
                            <option></option>
                            <optgroup label="{_ Logical status _}">
                                <option value="uncanceled">{_ Not canceled _}</option>
                                <option value="allaccess">{_ Access _}</option>
                                <option value="allactive">{_ Active _}</option>
                                <option value="allpending">{_ Pending _}</option>
                            </optgroup>
                            <optgroup label="{_ Exact status _}">
                                {% for s in m.paysub.subscriptions.states %}
                                    <option value="{{ s }}">{{ s }}</option>
                                {% endfor %}
                            </optgroup>
                        </select>
                    </div>
                    <div class="col-sm-2 col-xs-6">
                        <label class="control-label">{_ User group _}</label>
                        <select class="form-control" name="quser_group_id">
                            <option></option>
                            {% for id in m.paysub.subscriptions.user_groups|sort:'title' %}
                                <option value="{{ id }}">{{ id.title }}</option>
                            {% endfor %}
                        </select>
                    </div>
                    <div class="col-sm-2 col-xs-6">
                        <label class="control-label">{_ PSP _}</label>
                        <select class="form-control" name="qpsp">
                            <option></option>
                            {% for psp in m.paysub.subscriptions.psps %}
                                <option value="{{ psp|escape }}">{{ psp|escape|capfirst }}</option>
                            {% endfor %}
                        </select>
                    </div>
                    <div class="col-sm-3 col-xs-6">
                        <label class="control-label">{_ Product _}</label>
                        <select class="form-control" name="qproduct_id">
                            <option></option>
                            {% for psp, ps in m.paysub.subscriptions.products %}
                                {% if not q.qpsp or psp == q.qpsp %}
                                    <optgroup label="{{ psp|escape|capfirst }}">
                                        {% for p in ps %}
                                            <option value="{{ p.id }}">{{ p.name|escape }}</option>
                                        {% endfor %}
                                    </optgroup>
                                {% endif %}
                            {% endfor %}
                        </select>
                    </div>
                    <div class="col-sm-3 col-xs-6">
                        <label class="control-label">{_ Price _}</label>
                        <select class="form-control" name="qprice_id">
                            <option></option>
                            {% for psp, ps in m.paysub.subscriptions.prices %}
                                {% if not q.qpsp or psp == q.qpsp %}
                                    <optgroup label="{{ psp|escape|capfirst }}">
                                        {% for p in ps %}
                                            {% if not q.qproduct_id or q.qproduct_id == p.prod_id %}
                                                <option value="{{ p.id }}">
                                                    {{ p.prod_name|escape }}
                                                    {% if p.name != p.prod_name %}
                                                        ({{ p.name|escape }})
                                                    {% endif %}
                                                </option>
                                            {% endif %}
                                        {% endfor %}
                                    </optgroup>
                                {% endif %}
                            {% endfor %}
                        </select>
                    </div>
                </div>
            </form>

            {% javascript %}
                $('#filter-form').on('input', function(e) {
                    $(this).submit();
                    try { $('body').mask(); } catch (_) {};
                })
            {% endjavascript %}
        </div>


        {% with m.search.paged[{paysub_subscriptions
                    rsc_id=q.qrsc_id
                    price_id=q.qprice_id
                    product_id=q.qproduct_id
                    status=q.qstatus
                    user_group_id=q.quser_group_id
                    psp=q.qpsp
                    page=q.page
                    pagelen=20
            }] as result %}

            <p>{% trans "Found <b>{n}</b> subscriptions." n=result.total %}</p>

            {% include "_paysub_subscriptions_table.tpl" result=result %}
            {% pager result=result dispatch=`paysub_admin_subscriptions_overview` qargs hide_single_page %}
        {% endwith %}
    {% else %}
        <div class="alert alert-info">
            {_ Only administrators are allowed to see payments _}
        </div>
    {% endif %}

{% endblock %}
