CREATE TABLE IF NOT EXISTS terminal_affinity (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    customer_id UUID NOT NULL REFERENCES customer(id),
    provider_ref TEXT NOT NULL,
    terminal_ref TEXT NOT NULL,
    -- strict total order of bindings, immune to timestamp collisions
    bind_seq BIGSERIAL NOT NULL,
    bound_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- base for since_last_use; set on bind, bumped by every successful payment
    last_used_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    released_at TIMESTAMPTZ,
    released_reason TEXT
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_terminal_affinity_unique
    ON terminal_affinity(customer_id, provider_ref, terminal_ref)
    WHERE released_at IS NULL;

CREATE INDEX IF NOT EXISTS idx_terminal_affinity_lookup
    ON terminal_affinity(customer_id, bind_seq)
    WHERE released_at IS NULL;

-- for the admin "release everyone off terminal X" operation
CREATE INDEX IF NOT EXISTS idx_terminal_affinity_terminal
    ON terminal_affinity(provider_ref, terminal_ref)
    WHERE released_at IS NULL;
