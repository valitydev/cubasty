ALTER TABLE customer ADD COLUMN IF NOT EXISTS external_id TEXT;

CREATE UNIQUE INDEX IF NOT EXISTS idx_customer_external_id_party
    ON customer(external_id, party_ref)
    WHERE deleted_at IS NULL AND external_id IS NOT NULL;
