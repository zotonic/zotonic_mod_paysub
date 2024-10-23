%% @copyright 2022-2024 Marc Worrell
%% @doc Stripe support for payments and subscriptions.
%% @end

%% Copyright 2022-2024 Marc Worrrell
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

-module(paysub_schema).

-export([
    install/1
    ]).

-spec install(z:context()) -> ok.
install(Context) ->
    case z_db:table_exists(paysub_checkout, Context) of
        false ->
            ok = install_tables(Context),
            z_db:flush(Context),
            ok;
        true ->
            case z_db:table_exists(paysub_payment, Context) of
                false ->
                    [] = z_db:q("
                        create table paysub_payment (
                            id serial not null,
                            psp character varying(32) not null,
                            psp_payment_id character varying(128) not null,
                            psp_customer_id character varying(128),
                            psp_invoice_id character varying(128),
                            currency character varying(16) not null default 'EUR'::character varying,
                            amount integer not null,
                            amount_received integer not null,

                            name character varying(255),
                            email character varying(255),
                            phone character varying(255),

                            status character varying(32) not null default ''::character varying,
                            description character varying (300),

                            props_json jsonb,
                            created timestamp with time zone NOT NULL DEFAULT now(),
                            modified timestamp with time zone NOT NULL DEFAULT now(),

                            constraint paysub_payment_pkey primary key (id),
                            constraint paysub_payment_psp_payment_intent_id unique (psp, psp_payment_id)
                        )", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_psp_customer_key ON paysub_payment (psp, psp_customer_id)", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_modified_key ON paysub_payment (modified)", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_created_key ON paysub_payment (created)", Context),
                    z_db:flush(Context);
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_customer, phone, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_customer
                            add column address_street_1 character varying(255),
                            add column address_street_2 character varying(255),
                            add column address_city character varying(255),
                            add column address_state character varying(255),
                            add column address_postcode character varying(255),
                            add column phone character varying(255)
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_subscription, is_provisioned, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_subscription
                            add column is_provisioned boolean not null default false
                        ", Context),
                    z_db:flush(Context),
                    z_db:q("update paysub_subscription set is_provisioned = true", Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, psp_customer_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column psp_customer_id character varying(128)
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, survey_answer_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column survey_answer_id bigint
                        ", Context),
                    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_checkout_psp_psp_customer_id_key ON paysub_checkout (psp, psp_customer_id)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, survey_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column survey_id integer
                        ", Context),
                    [] = z_db:q("CREATE INDEX paysub_checkout_survey_id_key ON paysub_checkout (survey_id, survey_answer_id)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, psp_payment_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column psp_payment_id character varying(128)
                        ", Context),
                    [] = z_db:q("CREATE INDEX paysub_checkout_psp_psp_payment_id_key ON paysub_checkout (psp, psp_payment_id)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_product, is_use_maincontact, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_product
                            add column is_use_maincontact boolean not null default false
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_checkout, requestor_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_checkout
                            add column requestor_id integer
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_price, billing_scheme, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_price
                            add column billing_scheme character varying(32) not null default 'per_unit',
                            alter column amount drop not null
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_price, key, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_price
                            add column key character varying(128) not null default ''
                        ", Context),
                    [] = z_db:q("CREATE INDEX paysub_price_key_key ON paysub_price (key)", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_subscription_item, quantity, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_subscription_item
                            add column quantity integer
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end,
            case z_db:column_exists(paysub_subscription_item, psp_item_id, Context) of
                false ->
                    [] = z_db:q("
                            alter table paysub_subscription_item
                            add column psp_item_id character varying(128) not null default ''
                        ", Context),
                    z_db:flush(Context),
                    ok;
                true ->
                    ok
            end
    end.

install_tables(Context) ->
    [] = z_db:q("
        create table paysub_checkout (
            id serial not null,
            rsc_id integer,
            requestor_id integer,
            survey_id integer,
            survey_answer_id bigint,
            nr character varying(32) not null,
            psp character varying(32) not null,
            psp_checkout_id character varying(128),
            psp_payment_id character varying(128),
            psp_customer_id character varying(128),
            mode character varying(32) not null,
            status character varying(32) not null default 'open'::character varying,
            payment_status character varying(32) not null default 'open'::character varying,
            currency character varying(16),
            amount integer,
            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_checkout_pkey primary key (id),
            constraint paysub_checkout_nr unique (nr),
            constraint paysub_checkout_psp_id unique (psp, psp_checkout_id),
            constraint fk_paysub_checkout_rsc_id foreign key (rsc_id)
                references rsc (id)
                on update cascade
                on delete set null
        )", Context),
    [] = z_db:q("CREATE INDEX fki_paysub_checkout_rsc_id ON paysub_checkout (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_created_key ON paysub_checkout (created)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_modified_key ON paysub_checkout (modified)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_psp_psp_customer_id_key ON paysub_checkout (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_psp_psp_payment_id_key ON paysub_checkout (psp, psp_payment_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_checkout_survey_id_key ON paysub_checkout (survey_id, survey_answer_id)", Context),

    [] = z_db:q("
        create table paysub_product (
            id serial not null,
            is_active boolean not null default true,
            user_group_id int,
            is_use_maincontact boolean not null default false,
            name character varying(128) not null,
            psp character varying(32) not null,
            psp_product_id character varying(128) not null,
            psp_default_price_id character varying(128),
            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_product_pkey primary key (id),
            constraint paysub_product_psp_product_id unique (psp, psp_product_id)
        )", Context),

    [] = z_db:q("CREATE INDEX paysub_product_name_key ON paysub_product (name)", Context),

    [] = z_db:q("
        create table paysub_price (
            id serial not null,
            is_active boolean not null default true,
            name character varying(128) not null,
            key character varying(128) not null default '',
            psp character varying(32) not null,
            psp_price_id character varying(128) not null,
            psp_product_id character varying(128) not null,
            props_json jsonb,
            currency character varying(16) not null default 'EUR'::character varying,
            amount integer,
            billing_scheme character varying(32) not null default 'per_unit',
            is_recurring boolean not null default false,
            recurring_period character varying(16),
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_price_pkey primary key (id),
            constraint paysub_price_psp_price_id unique (psp, psp_price_id)
        )", Context),

    [] = z_db:q("CREATE INDEX paysub_price_name_key ON paysub_price (name)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_key_key ON paysub_price (key)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_psp_product_key ON paysub_price (psp, psp_product_id)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_created_key ON paysub_price (created)", Context),
    [] = z_db:q("CREATE INDEX paysub_price_modified_key ON paysub_price (modified)", Context),

    [] = z_db:q("
        create table paysub_subscription (
            id serial not null,
            rsc_id integer,
            psp character varying(32) not null,
            psp_subscription_id character varying(128) not null,
            psp_customer_id character varying(128) not null,
            is_provisioned boolean not null default false,
            status character varying(32) not null default ''::character varying,

            period_start timestamp with time zone,
            period_end timestamp with time zone,
            cancel_at timestamp with time zone,
            canceled_at timestamp with time zone,
            started_at timestamp with time zone,
            ended_at timestamp with time zone,
            trial_start timestamp with time zone,
            trial_end timestamp with time zone,

            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_pkey primary key (id),
            constraint paysub_psp_subscription_id unique (psp, psp_subscription_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_psp_customer_key ON paysub_subscription (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_status_key ON paysub_subscription (status)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_started_at_key ON paysub_subscription (started_at)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_ended_at_key ON paysub_subscription (ended_at)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_period_start_key ON paysub_subscription (period_start)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_period_end_at_key ON paysub_subscription (period_end)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_modified_key ON paysub_subscription (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_created_key ON paysub_subscription (created)", Context),

    [] = z_db:q("
        create table paysub_subscription_item (
            subscription_id int not null,
            psp character varying(32) not null,
            psp_item_id character varying(128) not null default '',
            psp_price_id character varying(128) not null,
            quantity integer,
            created timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_item_pkey primary key (subscription_id, psp, psp_price_id),
            constraint fk_paysub_subscription_item_subscription_id foreign key (subscription_id)
                references paysub_subscription (id)
                on update cascade
                on delete cascade
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS fki_paysub_subscription_item_subscription_id ON paysub_subscription_item (subscription_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_subscription_item_psp_psp_price_id_key ON paysub_subscription_item (psp, psp_price_id)", Context),

    [] = z_db:q("
        create table paysub_customer (
            id serial not null,
            rsc_id integer,
            psp character varying(32) not null,
            psp_customer_id character varying(128) not null,
            name character varying(255),
            email character varying(255),
            pref_language character varying(16),
            address_country character varying(16),
            address_street_1 character varying(255),
            address_street_2 character varying(255),
            address_city character varying(255),
            address_state character varying(255),
            address_postcode character varying(255),
            phone character varying(255),

            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_customer_pkey primary key (id),
            constraint paysub_customer_psp_customer_id unique (psp, psp_customer_id),
            constraint fk_paysub_customer_rsc_id foreign key (rsc_id)
                references rsc (id)
                on update cascade
                on delete set null
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS fki_paysub_customer_rsc_id ON paysub_customer (rsc_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_customer_modified_key ON paysub_customer (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_customer_created_key ON paysub_customer (created)", Context),

    % payment_status is one of: draft, open, paid, uncollectible, or void
    [] = z_db:q("
        create table paysub_invoice (
            id serial not null,
            psp character varying(32) not null,
            psp_invoice_id character varying(128) not null,
            psp_customer_id character varying(128) not null,
            currency character varying(16) not null default 'EUR'::character varying,
            amount_due integer not null,
            amount_paid integer not null,
            amount_remaining integer not null,
            total integer not null,

            collection_method character varying(32) not null default 'charge_automatically'::character varying,
            is_payment_attempted boolean not null default false,
            payment_method character varying(128),
            payment_status character varying(32) not null default ''::character varying,
            period_start timestamp with time zone,
            period_end timestamp with time zone,

            name character varying(255),
            email character varying(255),
            address_country character varying(16),
            address_street_1 character varying(255),
            address_street_2 character varying(255),
            address_city character varying(255),
            address_state character varying(255),
            address_postcode character varying(255),

            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_invoice_pkey primary key (id),
            constraint paysub_invoice_psp_invoice_id unique (psp, psp_invoice_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_psp_customer_key ON paysub_invoice (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_modified_key ON paysub_invoice (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_invoice_created_key ON paysub_invoice (created)", Context),

    % status is one of: requires_payment_method, requires_confirmation, requires_action, processing,
    % requires_capture, canceled, or succeeded
    [] = z_db:q("
        create table paysub_payment (
            id serial not null,
            psp character varying(32) not null,
            psp_payment_id character varying(128) not null,
            psp_customer_id character varying(128),
            psp_invoice_id character varying(128),
            currency character varying(16) not null default 'EUR'::character varying,
            amount integer not null,
            amount_received integer not null,

            name character varying(255),
            email character varying(255),
            phone character varying(255),

            status character varying(32) not null default ''::character varying,
            description character varying (300),

            props_json jsonb,
            created timestamp with time zone NOT NULL DEFAULT now(),
            modified timestamp with time zone NOT NULL DEFAULT now(),

            constraint paysub_payment_pkey primary key (id),
            constraint paysub_payment_psp_payment_intent_id unique (psp, psp_payment_id)
        )", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_psp_customer_key ON paysub_payment (psp, psp_customer_id)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_modified_key ON paysub_payment (modified)", Context),
    [] = z_db:q("CREATE INDEX IF NOT EXISTS paysub_payment_created_key ON paysub_payment (created)", Context),

    ok.
