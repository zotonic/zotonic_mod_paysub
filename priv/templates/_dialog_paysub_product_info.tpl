{% with m.paysub.product[q.product_id] as p %}
    {% if p %}
        {% wire id="paysub-product"
                type="submit"
                postback={product_update id=p.id on_success={reload}}
                delegate=`mod_paysub`
        %}
        <form id="paysub-product" action="postback">
            <table class="table">
                <tr>
                    <th>{_ Active _}</th>
                    <td>
                        <input type="checkbox" name="is_active" value="1" {% if p.is_active %}checked{% endif %}>
                        <span class="pull-right">{{ p.modified|date:"Y-m-d" }}
                            <span class="text-muted">{{ p.modified|date:"H:i"}}</span>
                        </span>
                    </td>
                </tr>
                <tr>
                    <th>{_ Name _}</th>
                    <td>
                        <div class="pull-right" style="padding-left:10px;">
                            <a href="{% url paysub_admin_subscriptions_overview qproduct_id=p.id %}">{_ Show all subscriptions _} &gt;</a>
                        </div>

                        {{ p.name|escape }}
                    </td>
                </tr>
                <tr>
                    <th>{_ Prices _}</th>
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
                                        {{ price.currency|replace:"EUR":"€"|escape }}&nbsp;{{ price.amount|format_price }}<br>
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
                </tr>
                <tr>
                    <th>{_ User group _}</th>
                    <td>
                        <select class="form-control" name="user_group_id">
                            <option value=""></option>
                            {% for ug in m.hierarchy.acl_user_group.tree_flat %}
                                <option value="{{ ug.id }}" {% if ug.id == p.user_group_id %}selected{% endif %}>
                                    {{ ug.indent }}{{ ug.id.title }}
                                </option>
                            {% endfor %}
                        </select>

                        <p class="help-block">
                            {_ Subscribers to this product are added to this user group. _}
                        </p>
                    </td>
                </tr>
                <tr>
                    <th>{_ PSP _}</th>
                    <td>
                        {# TODO: make flexible for PSP #}
                        {% if p.psp == 'stripe' %}
                            <a href="https://dashboard.stripe.com/products/{{ p.psp_product_id|urlencode }}"
                               class="btn btn-primary btn-xs" target="payment-psp">
                                <span class="glyphicon glyphicon-new-window"></span>
                                {% trans "view at {psp}" psp= p.psp|capfirst|escape %}
                            </a>
                        {% else %}
                            {{ p.psp|escape }}
                            <span class="text-muted">| {{ p.psp_product_id|escape }}</span>
                        {% endif %}
                    </td>
                </tr>
            </table>

            <p class="help-block">
                {_ If this product is changed at the Payment Service Provider then the above fields could be overwritten with information from the PSP. _}
            </p>

            <div class="modal-footer">
                {% button class="btn btn-default" action={dialog_close} text=_"Close" tag="a" %}
                <button class="btn btn-success" type="submit">{_ Save _}</button>
            </div>
        </form>
    {% else %}
        <p class="text-muted">
            {_ Unknown product, or no permission to view this product. _}
        </p>
        <div class="modal-footer">
            {% button class="btn btn-primary" action={dialog_close} text=_"Close" tag="a" %}
        </div>
    {% endif %}


{% endwith %}
