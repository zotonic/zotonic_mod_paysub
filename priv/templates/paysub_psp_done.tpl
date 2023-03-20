{% extends "page.tpl" %}

{% block html_head_extra %}
{% lib
    "css/z.icons.css"
    "css/logon.css"
%}
{% endblock %}

{#
Page to show when returned from PSP

- Url is "/payment/done/<payment-nr>"
- Uses controller_template
- Check status of payment, show 'paid', 'error', or 'pending' page.
- Those statuses are blocks, so easy to overrule.

(Note that any connected actions are already performed, as a result of paysub_status notifications)
#}

{% block content %}
    <div class="z-logon-box">
        {% with m.paysub.checkout.status[q.checkout_nr] as payment %}

            {% if is_canceled %}
                {% block payment_canceled %}
                    <p>
                        {_ Your payment was canceled. _}
                    </p>
                {% endblock %}
            {% else %}
                {% if not payment %}
                    <p class="alert alert-error">{_ Unknown payment _}</p>
                {% elseif payment.is_failed %}
                    {% block payment_failed %}
                        <p>
                            {_ Your payment was not handled. _}
                        </p>
                    {% endblock %}
                {% else %}
                    {% if payment.is_paid %}
                        {% block payment_paid %}
                            <h2>
                                {_ Thank you for your payment! _}
                            </h2>
                        {% endblock %}
                    {% else %}
                        {% block payment_pending %}
                            <h2>
                                {_ Your payment is pending. _}
                            </h2>
                        {% endblock %}
                    {% endif %}

                    {% if     payment.user_id
                          and payment.user_info.is_expired
                          and not payment.user_info.visited
                    %}
                        <p>{_ Please set your username and password. _}</p>

                        {% wire id="signup-pw-reset"
                                type="submit"
                                action={hide target="err-username"}
                                postback={set_usernamepw checkout_nr=q.checkout_nr user_id=payment.user_id}
                                delegate=`mod_paysub`
                        %}
                        <form id="signup-pw-reset" class="form" action="postback">
                            <div class="form-group">
                                <label for="username" class="control-label">{_ Username _}</label>
                                <input type="text" id="username" name="username" class="form-control" required inputmode="email" autocapitalize="off" autocorrect="off" autocomplete="username" value="{{ payment.user_info.username|escape }}">
                                {% validate id="username" type={presence} %}
                                <p class="alert alert-error" style="display: none" id="err-username">
                                    {_ Sorry, this username is already in use. _}
                                </p>
                            </div>
                            <div class="form-group">
                                <label for="password" class="control-label">{_ Password _}</label>
                                <input id="password" type="password" name="password" autocomplete="new-password" class="form-control" required autofocus>
                                {% validate id="password" type={presence} %}
                            </div>
                            <div class="form-group">
                                <button class="btn btn-primary" type="submit">{_ Save and continue _}</button>
                            </div>
                        </form>
                    {% endif %}
                {% endif %}

            {% endif %}

            {% block payment_below %}
                {% if not payment.user_info.is_expired and payment.user_id %}
                    {% if payment.user_id %}
                        <a class="btn btn-default" href="{{ payment.user_id.page_url }}">{_ Go to your profile page _}</a>
                    {% else %}
                        <a class="btn btn-default" href="{% url home %}">{_ Back to home _}</a>
                    {% endif %}
                {% endif %}
            {% endblock %}

        {% endwith %}
    </div>

{% endblock %}

