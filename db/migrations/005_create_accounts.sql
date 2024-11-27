CREATE TABLE bank_accounts (
    account_id UUID PRIMARY KEY NOT NULL,
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_account_numbers (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    account_number VARCHAR(20) UNIQUE NOT NULL,
    assigned_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE pending_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    pended_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE approved_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    approved_at TIMESTAMPTZ DEFAULT NOW(),
    approval_reason TEXT
);

CREATE TABLE active_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    activated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE suspended_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    suspended_at TIMESTAMPTZ DEFAULT NOW(),
    suspension_reason TEXT
);

CREATE TABLE closed_accounts (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    closed_at TIMESTAMPTZ DEFAULT NOW(),
    closure_reason TEXT
);

CREATE INDEX idx_bank_account_numbers_account_number ON bank_account_numbers(account_number);
CREATE INDEX idx_bank_accounts_user_id ON bank_accounts(user_id);
CREATE INDEX idx_pending_accounts_account_id ON pending_accounts(account_id);
CREATE INDEX idx_active_accounts_account_id ON active_accounts(account_id);
CREATE INDEX idx_suspended_accounts_account_id ON suspended_accounts(account_id);
CREATE INDEX idx_suspended_accounts_suspended_at ON suspended_accounts(suspended_at);
CREATE INDEX idx_closed_accounts_account_id ON closed_accounts(account_id);
CREATE INDEX idx_closed_accounts_closed_at ON closed_accounts(closed_at);
CREATE INDEX idx_approved_accounts_account_id ON approved_accounts(account_id);
CREATE INDEX idx_approved_accounts_approved_at ON approved_accounts(approved_at);
