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

                        {% if payment.args.cancel_url %}
                            <p>
                                <a class="btn btn-default" href="{{ payment.args.cancel_url|escape }}">{_ Back _}</a>
                            </p>
                        {% endif %}
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
                    {% block payment_done %}
                        {% if     payment.user_id
                              and payment.user_info.is_expired
                              and not payment.user_info.visited
                        %}
                            {# An account has been created, with the password set to expired
                             # Show the form to set the username/password before continuing.
                             #}
                            <p>
                                {_ Before you continue, please set your username and password. _}
                                {_ You can always log on using your email address. _}
                            </p>

                            {% if payment.args.success_url as url %}
                                {% wire id="signup-pw-reset"
                                        type="submit"
                                        action={hide target="err-username"}
                                        postback={set_usernamepw
                                                checkout_nr=q.checkout_nr
                                                user_id=payment.user_id
                                                url=url}
                                        delegate=`mod_paysub`
                                %}
                            {% elseif payment.args.referring_id.payment_done_html %}
                                {# After setting password, continue to info page of referring_id #}
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
                                {# After setting password, continue to user page #}
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
                                    <label for="username" class="control-label">{_ New username _}</label>
                                    <input type="text" id="username" name="username" class="form-control" required inputmode="email" autocapitalize="off" autocorrect="off" autocomplete="username" value="{{ payment.user_info.username|escape }}">
                                    {% validate id="username" type={presence} %}
                                    <p class="alert alert-error" style="display: none" id="err-username">
                                        {_ Sorry, this username is already in use. _}
                                    </p>
                                </div>
                                <div class="form-group">
                                    <label for="password" class="control-label">{_ New password _}</label>
                                    <input id="password" type="password" name="password" autocomplete="new-password" class="form-control" required autofocus
                                    {% if m.authentication.password_min_length as min %}minlength="{{ min }}"{% endif %}
                                    {% if m.authentication.password_regex as regex %}pattern="{{ regex|escape }}"{% endif %}>
                                    {% validate id="password"
                                                type={acceptable_password
                                                    message=_"Your new password is too short or not strong enough. Use a: uppercase letter, lowercase letter, number, symbol."
                                                }
                                                only_on_blur
                                    %}
                                </div>
                                <div class="form-group">
                                    <button class="btn btn-primary" type="submit">{_ Save and continue _}</button>
                                </div>
                            </form>
                        {% elseif payment.args.success_url as url %}
                            {# Redirect to the payment success page. #}
                            {% wire action={redirect dispatch=`paysub_psp_done_welcome` location=url} %}
                        {% elseif payment.args.referring_id.payment_done_html %}
                            {# Redirect to the page with the information about the payment. #}
                            {% wire action={redirect dispatch=`paysub_psp_done_welcome` id=payment.args.referring_id} %}
                        {% else %}
                            {# Show buttons to go to the profile page or back. #}
                            {% if payment.user_id %}
                                <a class="btn btn-primary" href="{{ payment.user_id.page_url }}">{_ Continue to your profile page _}</a>
                            {% endif %}
                            {% if payment.args.referring_id %}
                                <a class="btn btn-primary" href="{{ referring_id.page_url }}">{_ Back _}</a>
                            {% else %}
                                <a class="btn btn-primary" href="{% url home %}">{_ Back to home _}</a>
                            {% endif %}
                        {% endif %}
                    {% endblock %}
                {% endif %}
            {% endif %}

        {% endwith %}
    </div>

{% endblock %}

