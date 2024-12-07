CREATE TABLE bank_accounts (
    account_id UUID PRIMARY KEY NOT NULL,
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TYPE bank_account_status_enum AS ENUM (
    'Pending',
    'Approved',
    'Active',
    'Suspended',
    'Closed'
);

CREATE TABLE bank_account_status (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    status VARCHAR(50) NOT NULL,
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

CREATE TABLE bank_account_credentials (
    account_id UUID PRIMARY KEY REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    password_hash TEXT NOT NULL,
    expires_at TIMESTAMPTZ NOT NULL, 
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_account_password_history (
    history_id SERIAL PRIMARY KEY,
    account_id UUID NOT NULL REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    password_hash TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_account_auth_failures (
    failure_id SERIAL PRIMARY KEY,
    account_id UUID REFERENCES bank_accounts(account_id) ON DELETE CASCADE,
    failure_timestamp TIMESTAMPTZ DEFAULT NOW(),
    ip_address INET,
    user_agent TEXT,
    failure_reason TEXT
);

CREATE TABLE audit_logs (
    id                  SERIAL PRIMARY KEY,
    url                 TEXT NOT NULL,
    method              VARCHAR(10) NOT NULL, 
    user_id             INT REFERENCES users(id) ON DELETE SET NULL, 
    description         TEXT,
    parameters          JSONB,
    query               JSONB,
    ip_address          INET,
    user_agent          TEXT,
    operation_type      VARCHAR(50) NOT NULL DEFAULT 'Unknown', 
    transaction_id      UUID NOT NULL DEFAULT gen_random_uuid(),
    response_status     INT, 
    response_message    TEXT,
    request_started_at  TIMESTAMPTZ NOT NULL,
    request_ended_at    TIMESTAMPTZ,
    created_at          TIMESTAMPTZ DEFAULT NOW(), 
    updated_at          TIMESTAMPTZ DEFAULT NOW()  
);

CREATE INDEX idx_bank_accounts_user_id ON bank_accounts(user_id);
CREATE INDEX idx_bank_account_numbers_account_number ON bank_account_numbers(account_number);
CREATE INDEX idx_bank_account_status_account_id ON bank_account_status(account_id);
CREATE INDEX idx_bank_account_status_updated_at ON bank_account_status(updated_at);
CREATE INDEX idx_pending_accounts_account_id ON pending_accounts(account_id);
CREATE INDEX idx_approved_accounts_account_id ON approved_accounts(account_id);
CREATE INDEX idx_approved_accounts_approved_at ON approved_accounts(approved_at);
CREATE INDEX idx_active_accounts_account_id ON active_accounts(account_id);
CREATE INDEX idx_suspended_accounts_account_id ON suspended_accounts(account_id);
CREATE INDEX idx_suspended_accounts_suspended_at ON suspended_accounts(suspended_at);
CREATE INDEX idx_closed_accounts_account_id ON closed_accounts(account_id);
CREATE INDEX idx_closed_accounts_closed_at ON closed_accounts(closed_at);
CREATE INDEX idx_bank_account_credentials_account_id ON bank_account_credentials(account_id);
CREATE INDEX idx_bank_account_credentials_expires_at ON bank_account_credentials(expires_at);
CREATE INDEX idx_bank_account_password_history_account_id ON bank_account_password_history(account_id);
CREATE INDEX idx_bank_account_auth_failures_account_id ON bank_account_auth_failures(account_id);
CREATE INDEX idx_bank_account_auth_failures_failure_timestamp ON bank_account_auth_failures(failure_timestamp);
CREATE INDEX idx_audit_logs_user_id ON audit_logs(user_id);
CREATE INDEX idx_audit_logs_transaction_id ON audit_logs(transaction_id);
CREATE INDEX idx_audit_logs_request_started_at ON audit_logs(request_started_at);
CREATE INDEX idx_audit_logs_request_ended_at ON audit_logs(request_ended_at);
