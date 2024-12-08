CREATE TABLE bank_balance (
    balance_id SERIAL PRIMARY KEY,
    account_id UUID REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    currency_code CHAR(3) NOT NULL DEFAULT 'JPY',
    balance NUMERIC(15, 2) NOT NULL,
    balance_timestamp TIMESTAMPTZ DEFAULT NOW(),
    CONSTRAINT unique_account_id UNIQUE (account_id) 
);

CREATE TABLE bank_balance_history (
    history_id SERIAL PRIMARY KEY,
    account_id UUID REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    currency_code CHAR(3) NOT NULL DEFAULT 'JPY',
    transaction_type VARCHAR(50),
    balance NUMERIC(15, 2) NOT NULL,
    balance_timestamp TIMESTAMPTZ DEFAULT NOW(),
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_account_balance ON bank_balance (account_id, balance_timestamp DESC);
CREATE INDEX idx_bank_balance_history_account_id ON bank_balance_history(account_id);
CREATE INDEX idx_bank_balance_history_balance_timestamp ON bank_balance_history(balance_timestamp DESC);
