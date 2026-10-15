-- A payment is seen only in the party it was made in.
-- Nullable while pods that do not write it are still running; their rows are seen in no
-- party until this file is run again.
ALTER TABLE payment_ref ADD COLUMN IF NOT EXISTS party_id BIGINT REFERENCES party(id);

CREATE INDEX IF NOT EXISTS idx_payment_ref_customer_party ON payment_ref(customer_id, party_id);

-- Until now a Customer was in a single party, the one it was created in, and its payments
-- were made there.
UPDATE payment_ref pr
SET party_id = p.id
FROM customer c
JOIN party p ON p.party_ref = c.party_ref
WHERE c.id = pr.customer_id
  AND pr.party_id IS NULL;
