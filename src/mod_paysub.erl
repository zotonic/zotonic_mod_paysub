%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2022 Marc Worrell
%% @doc Subscriptions and payments for members using Stripe and other PSPs

%% Copyright 2022 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_paysub).

-mod_title("Paid subscriptions").
-mod_description("Paid subscriptions for members").
-mod_author("Marc Worrell <marc@worrell.nl>").
-mod_depends([]).
-mod_schema(1).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,

    manage_schema/2
]).

% -include_lib("kernel/include/logger.hrl").

init(Context) ->
    m_config:set_default_value(?MODULE, stripe_api_key, <<>>, Context),
    m_config:set_default_value(?MODULE, stripe_webhook_secret, <<>>, Context),
    ok.

manage_schema(_Version, Context) ->
    m_paysub:install(Context).

