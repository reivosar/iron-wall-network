CREATE TABLE bank_accounts (
    account_id UUID PRIMARY KEY NOT NULL,
    account_number VARCHAR(20) UNIQUE NOT NULL,
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE pending_account (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    pended_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE active_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    activated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE suspend_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    suspended_at TIMESTAMPTZ DEFAULT NOW(),
    suspension_reason TEXT
);

CREATE TABLE closed_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    closed_at TIMESTAMPTZ DEFAULT NOW(),
    closure_reason TEXT
);

CREATE INDEX idx_bank_accounts_account_number ON bank_accounts(account_number);
CREATE INDEX idx_bank_accounts_user_id ON bank_accounts(user_id);
CREATE INDEX idx_pending_account_account_id ON pending_account(account_id);
CREATE INDEX idx_active_accounts_account_id ON active_accounts(account_id);
CREATE INDEX idx_suspend_accounts_account_id ON suspend_accounts(account_id);
CREATE INDEX idx_suspend_accounts_suspended_at ON suspend_accounts(suspended_at);
CREATE INDEX idx_closed_accounts_account_id ON closed_accounts(account_id);
CREATE INDEX idx_closed_accounts_closed_at ON closed_accounts(closed_at);
