-- Withdrawals of a Customer. A withdrawal belongs to one Customer and is seen only in the
-- party it was made in.
CREATE TABLE IF NOT EXISTS withdrawal_ref (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id UUID NOT NULL REFERENCES customer(id),
    party_id BIGINT NOT NULL REFERENCES party(id),
    withdrawal_id TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_withdrawal_ref_withdrawal ON withdrawal_ref(withdrawal_id);
CREATE INDEX IF NOT EXISTS idx_withdrawal_ref_customer ON withdrawal_ref(customer_id, party_id);
