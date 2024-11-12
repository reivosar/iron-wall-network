CREATE TABLE bank_balance (
    balance_id SERIAL PRIMARY KEY,
    account_id INT REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    balance NUMERIC(15, 2) NOT NULL,
    balance_timestamp TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_account_balance ON bank_balance (account_id, balance_timestamp DESC);