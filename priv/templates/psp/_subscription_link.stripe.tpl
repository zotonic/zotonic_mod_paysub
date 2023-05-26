<a href="https://dashboard.stripe.com/subscriptions/{{ sub.psp_subscription_id|urlencode }}"
   class="btn btn-primary btn-xs" target="payment-psp">
    <span class="glyphicon glyphicon-new-window"></span>
    {% trans "view at {psp}" psp=sub.psp|capfirst|escape %}
</a>
