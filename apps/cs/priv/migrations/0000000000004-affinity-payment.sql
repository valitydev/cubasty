-- payment_ref is already keyed by the payment (idx_payment_ref_invoice), so it is the
-- ledger of payments a Customer has been through. This column records which binding a
-- payment produced, turning that ledger into the idempotency journal of
-- BindTerminalAffinity: a bind whose payment is already in the ledger returns the
-- binding that payment made and touches nothing.
-- Nullable and without a default: rows written before this migration, and rows written
-- by AddPayment, are payments that produced no binding.
ALTER TABLE payment_ref
    ADD COLUMN IF NOT EXISTS terminal_affinity_id UUID REFERENCES terminal_affinity(id);
