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

(Note that any connected actions are already performed, as a result of paysub_subscription notifications)
#}

{% block content %}
    <div class="z-logon-box">
        {% with m.paysub.checkout.status[q.checkout_nr] as payment %}
            {% if is_canceled %}
                {% block payment_canceled %}
                    <h2>
                        {_ Your payment was canceled. _}
                    </h2>
                    <p>{_ Continue reading our site. _}</p>

                    {% if payment.args.cancel_url %}
                        <p>
                            <a class="btn btn-default" href="{{ payment.args.cancel_url|escape }}">{_ Back _}</a>
                        </p>
                    {% endif %}
                {% endblock %}
            {% else %}
                {% if not payment %}
                    <h2>
                        {_ Something went wrong _}
                    </h2>

                    <p class="alert alert-error">{_ Unknown payment _}</p>

                    {% if payment.args.cancel_url %}
                        <p>
                            <a class="btn btn-default" href="{{ payment.args.cancel_url|escape }}">{_ Back _}</a>
                        </p>
                    {% endif %}
                {% elseif payment.is_failed %}
                    {% block payment_failed %}
                        <h2>
                            {_ Something went wrong _}
                        </h2>
                        <p>
                            {_ Your payment was not handled. _}
                        </p>
                    {% endblock %}
                {% else %}
                    {% if payment.is_paid %}
                        {% block payment_paid %}
                            <h2>
                                {_ Thank you! _}
                            </h2>
                            <p>{_ Your payment was handled succesfully. _}</p>
                        {% endblock %}
                    {% else %}
                        {% block payment_pending %}
                            <h2>
                                {_ Thank you! _}
                            </h2>
                            <p>{_ Your payment is pending. _}</p>
                        {% endblock %}
                    {% endif %}
                    {% if     payment.user_id
                          and payment.user_info.is_expired
                          and not payment.user_info.visited
                    %}
                        <p>
                            {_ Before you continue, please set your username and password. _}
                            {_ You can always log on using your email address. _}
                        </p>

                        {% if payment.args.referring_id.payment_done_html %}
                            {% wire id="signup-pw-reset"
                                    type="submit"
                                    action={hide target="err-username"}
                                    postback={set_usernamepw
                                            checkout_nr=q.checkout_nr
                                            user_id=payment.user_id
                                            url={paysub_psp_done_welcome id=payment.args.referring_id}|url}
                                    delegate=`mod_paysub`
                            %}
                        {% else %}
                            {% wire id="signup-pw-reset"
                                    type="submit"
                                    action={hide target="err-username"}
                                    postback={set_usernamepw
                                            checkout_nr=q.checkout_nr
                                            user_id=payment.user_id}
                                    delegate=`mod_paysub`
                            %}
                        {% endif %}
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
                                <input id="password" type="password" name="password" autocomplete="new-password" class="form-control" required autofocus
                                {% if m.authentication.password_min_length as min %}minlength="{{ min }}"{% endif %}
                                {% if m.authentication.password_regex as regex %}pattern="{{ regex|escape }}"{% endif %}>
                                {% if m.admin_identity.password_regex %}
                                    {% validate id="password" type={format pattern=m.admin_identity.password_regex failure_message=_"This password does not meet the security requirements"} type={presence} %}
                                {% else %}
                                    {% validate id="password" type={length minimum=m.authentication.password_min_length} type={presence} %}
                                {% endif %}
                            </div>
                            <div class="form-group">
                                <button class="btn btn-primary" type="submit">{_ Save and continue _}</button>
                            </div>
                        </form>
                    {% elseif payment.args.referring_id.payment_done_html %}
                        WOULD REDIRECT
                        {#
                        {% wire action={redirect dispatch=`paysub_psp_done_welcome` id=payment.args.referring_id} %}
                        #}
                    {% endif %}
                {% endif %}

            {% endif %}

            {% block payment_below %}
                {% if not payment.user_info.is_expired and payment.user_id %}
                    {% if payment.user_id %}
                        {% if payment.args.referring_id.payment_done_html %}
                            <a class="btn btn-primary" href="{% url paysub_psp_done_welcome id=payment.args.referring_id %}">{_ Continue _}</a>
                        {% else %}
                            <a class="btn btn-primary" href="{{ payment.user_id.page_url }}">{_ Continue to your profile page _}</a>
                        {% endif %}
                    {% else %}
                        <a class="btn btn-primary" href="{% url home %}">{_ Back to home _}</a>
                    {% endif %}
                {% endif %}
            {% endblock %}

        {% endwith %}
    </div>

{% endblock %}

