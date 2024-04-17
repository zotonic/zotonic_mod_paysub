{% overrules %}

{% block meta_after %}
    <div class="form-group">
        <label class="col-sm-3 control-label">{_ Subscription status_}</label>
        <div class="col-sm-9">
            <select name="qpaysub_subscription_status" class="form-control col-md-8">
                <option value=""></option>
                <optgroup label="{_ Active/inactive _}">
                    {% if m.paysub.is_unpaid_access %}
                        <option value="all_access" {% if q.qpaysub_subscription_status == 'all_access' %}selected{% endif %}>
                            {_ Access subscription _}
                        </option>
                        <option value="all_pending" {% if q.qpaysub_subscription_status == 'all_pending' %}selected{% endif %}>
                            {_ Pending subscription _}
                        </option>
                    {% endif %}

                    <option value="all_active" {% if q.qpaysub_subscription_status == 'all_active' %}selected{% endif %}>
                        {_ Active subscription _}
                    </option>
                    <option value="all_inactive" {% if q.qpaysub_subscription_status == 'all_inactive' %}selected{% endif %}>
                        {_ Inactive subscription _}
                    </option>
                    <option value="pastdue_noaccess" {% if q.qpaysub_subscription_status == 'pastdue_noactive' %}selected{% endif %}>
                        {_ Past due without active _}
                    </option>
                    <option value="incomplete_noaccess" {% if q.qpaysub_subscription_status == 'incomplete_noaccess' %}selected{% endif %}>
                        {_ Incomplete without other access _}
                    </option>
                </optgroup>
                <optgroup label="{_ New active subscriptions _}">
                    <option value="new_w1" {% if q.qpaysub_subscription_status == 'new_w1' %}selected{% endif %}>
                        {_ New in last week _}
                    </option>
                    <option value="new_m1" {% if q.qpaysub_subscription_status == 'new_m1' %}selected{% endif %}>
                        {_ New in last month _}
                    </option>
                    <option value="new_y1" {% if q.qpaysub_subscription_status == 'new_y1' %}selected{% endif %}>
                        {_ New in last year _}
                    </option>
                </optgroup>
                <optgroup label="{_ Cancelations _}">
                    <option value="canceled_now" {% if q.qpaysub_subscription_status == 'canceled_now' %}selected{% endif %}>
                        {_ Canceling at this moment _}
                    </option>
                    <option value="canceled_m1" {% if q.qpaysub_subscription_status == 'canceled_m1' %}selected{% endif %}>
                        {_ Canceled in last month _}
                    </option>
                    <option value="canceled_y1" {% if q.qpaysub_subscription_status == 'canceled_y1' %}selected{% endif %}>
                        {_ Canceled in last year _}
                    </option>
                </optgroup>
                <optgroup label="{_ Specific at any time _}">
                    {% for s, t in [
                        ['incomplete', _"Incomplete"],
                        ['incomplete_expired', _"Incomplete expired"],
                        ['trialing', _"Trialing"],
                        ['active', _"Active"],
                        ['past_due', _"Past due"],
                        ['canceled', _"Canceled"],
                        ['unpaid', _"Unpaid"]
                    ] %}
                        <option value="{{ s }}" {% if q.qpaysub_subscription_status == s %}selected{% endif %}>
                            {{ t }}
                        </option>
                    {% endfor %}
                </optgroup>
            </select>
        </div>
    </div>
{% endblock %}
