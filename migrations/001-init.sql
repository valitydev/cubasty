-- :up

CREATE TABLE customer (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    party_ref TEXT NOT NULL,
    contact_info JSONB,
    metadata JSONB,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    deleted_at TIMESTAMPTZ
);

CREATE INDEX idx_customer_party ON customer(party_ref);
CREATE INDEX idx_customer_active ON customer(id) WHERE deleted_at IS NULL;

CREATE TABLE bank_card (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    bank_card_token TEXT NOT NULL,
    card_mask TEXT,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    deleted_at TIMESTAMPTZ
);

CREATE UNIQUE INDEX idx_bank_card_token ON bank_card(bank_card_token) WHERE deleted_at IS NULL;

CREATE TABLE customer_bank_card (
    customer_id UUID REFERENCES customer(id),
    bank_card_id UUID REFERENCES bank_card(id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    deleted_at TIMESTAMPTZ,
    PRIMARY KEY (customer_id, bank_card_id)
);

CREATE INDEX idx_customer_bank_card_customer ON customer_bank_card(customer_id) WHERE deleted_at IS NULL;

CREATE TABLE bank_card_party (
    bank_card_id UUID REFERENCES bank_card(id),
    party_ref TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    deleted_at TIMESTAMPTZ,
    PRIMARY KEY (bank_card_id, party_ref)
);

CREATE INDEX idx_bank_card_party_lookup ON bank_card_party(party_ref) WHERE deleted_at IS NULL;

CREATE TABLE recurrent_token (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    bank_card_id UUID REFERENCES bank_card(id),
    provider_ref TEXT NOT NULL,
    terminal_ref TEXT NOT NULL,
    token TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    invalidated_at TIMESTAMPTZ,
    invalidated_reason TEXT
);

CREATE INDEX idx_recurrent_token_card ON recurrent_token(bank_card_id);
CREATE INDEX idx_recurrent_token_active ON recurrent_token(bank_card_id, provider_ref, terminal_ref)
    WHERE invalidated_at IS NULL;

CREATE TABLE payment_ref (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id UUID REFERENCES customer(id),
    invoice_id TEXT NOT NULL,
    payment_id TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE UNIQUE INDEX idx_payment_ref_invoice ON payment_ref(invoice_id, payment_id);
CREATE INDEX idx_payment_ref_customer ON payment_ref(customer_id);

-- :down

DROP INDEX IF EXISTS idx_payment_ref_customer;
DROP INDEX IF EXISTS idx_payment_ref_invoice;
DROP INDEX IF EXISTS idx_recurrent_token_active;
DROP INDEX IF EXISTS idx_recurrent_token_card;
DROP INDEX IF EXISTS idx_bank_card_party_lookup;
DROP INDEX IF EXISTS idx_customer_bank_card_customer;
DROP INDEX IF EXISTS idx_bank_card_token;
DROP INDEX IF EXISTS idx_customer_active;
DROP INDEX IF EXISTS idx_customer_party;

DROP TABLE IF EXISTS payment_ref;
DROP TABLE IF EXISTS recurrent_token;
DROP TABLE IF EXISTS bank_card_party;
DROP TABLE IF EXISTS customer_bank_card;
DROP TABLE IF EXISTS bank_card;
DROP TABLE IF EXISTS customer;
