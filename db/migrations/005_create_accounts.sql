CREATE TABLE bank_accounts (
    account_id SERIAL PRIMARY KEY,
    account_number VARCHAR(20) UNIQUE NOT NULL,
    account_holder_name VARCHAR(255) NOT NULL,
    user_id INT NOT NULL REFERENCES bank_users(user_id),
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE account_pending (
    pending_id SERIAL PRIMARY KEY,
    account_id INT REFERENCES bank_accounts(account_id),
    pending_reason TEXT,
    pended_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE account_activated (
    activated_id SERIAL PRIMARY KEY,
    account_id INT REFERENCES bank_accounts(account_id),
    activated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE account_suspended (
    suspended_id SERIAL PRIMARY KEY,
    account_id INT REFERENCES bank_accounts(account_id),
    suspended_at TIMESTAMPTZ DEFAULT NOW(),
    suspension_reason TEXT
);

CREATE INDEX idx_bank_accounts_account_number ON bank_accounts(account_number);
CREATE INDEX idx_account_pending_account_id ON account_pending(account_id);
CREATE INDEX idx_account_activated_account_id ON account_activated(account_id);
CREATE INDEX idx_account_suspended_account_id ON account_suspended(account_id);
