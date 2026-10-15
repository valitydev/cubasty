-- Parties known to the service. Parties themselves live in the domain config: a row is
-- registered the first time a party is seen here, and nothing removes it.
CREATE TABLE IF NOT EXISTS party (
    id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
    party_ref TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Parties a Customer belongs to, along with the external id the party knows it by. A
-- Customer belongs to the party it was created in and may be linked to others.
-- Supersedes customer.party_ref and customer.external_id.
CREATE TABLE IF NOT EXISTS customer_party (
    customer_id UUID NOT NULL REFERENCES customer(id),
    party_id BIGINT NOT NULL REFERENCES party(id),
    external_id TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at TIMESTAMPTZ
);

-- a Customer belongs to a party once
CREATE UNIQUE INDEX IF NOT EXISTS idx_customer_party_customer
    ON customer_party(customer_id, party_id)
    WHERE deleted_at IS NULL;

-- within a party an external id points at one Customer
CREATE UNIQUE INDEX IF NOT EXISTS idx_customer_party_external_id
    ON customer_party(party_id, external_id)
    WHERE deleted_at IS NULL AND external_id IS NOT NULL;

-- Customers of a party
CREATE INDEX IF NOT EXISTS idx_customer_party_party
    ON customer_party(party_id)
    WHERE deleted_at IS NULL;

COMMENT ON COLUMN customer.party_ref IS
    'Deprecated: the party the Customer was created in. Parties of a Customer are in customer_party. Still scopes customer.email.';
COMMENT ON COLUMN customer.external_id IS
    'Deprecated: neither read nor written. External ids are in customer_party.';

INSERT INTO party (party_ref)
SELECT DISTINCT c.party_ref
FROM customer c
ON CONFLICT (party_ref) DO NOTHING;

-- deleted Customers come along with their deleted_at, so they keep not holding their ids
INSERT INTO customer_party (customer_id, party_id, external_id, created_at, deleted_at)
SELECT c.id, p.id, c.external_id, COALESCE(c.created_at, NOW()), c.deleted_at
FROM customer c
JOIN party p ON p.party_ref = c.party_ref
WHERE NOT EXISTS (SELECT 1 FROM customer_party cp WHERE cp.customer_id = c.id);

-- Customers deleted without their links, by a release predating this table, free their
-- external ids. Like the inserts above, this is safe to run again after a rolling deploy.
UPDATE customer_party cp
SET deleted_at = c.deleted_at
FROM customer c
WHERE c.id = cp.customer_id
  AND c.deleted_at IS NOT NULL
  AND cp.deleted_at IS NULL;
