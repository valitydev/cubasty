ALTER TABLE customer ADD COLUMN IF NOT EXISTS email TEXT;

-- email is stored already normalized (lower + trim), hence a plain column index
-- rather than an expression one; partial, same as external_id
CREATE UNIQUE INDEX IF NOT EXISTS idx_customer_email_party
    ON customer(email, party_ref)
    WHERE deleted_at IS NULL AND email IS NOT NULL;
