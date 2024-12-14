CREATE TABLE audit_logs (
    id                  SERIAL PRIMARY KEY,
    transaction_id      UUID NOT NULL DEFAULT gen_random_uuid(),
    operator_id         INT,
    ip_address          INET,
    user_agent          TEXT,
    description         TEXT,
    url                 TEXT NOT NULL,
    content_type        TEXT, 
    method              VARCHAR(10) NOT NULL, 
    parameters          JSONB,
    query               JSONB,
    response_status     INT, 
    response_message    TEXT,
    request_started_at  TIMESTAMPTZ NOT NULL,
    request_ended_at    TIMESTAMPTZ
);

CREATE INDEX idx_audit_logs_transaction_id ON audit_logs (transaction_id);
CREATE INDEX idx_audit_logs_operator_id ON audit_logs (operator_id);
CREATE INDEX idx_audit_logs_request_started_at ON audit_logs (request_started_at);
CREATE INDEX idx_audit_logs_url_method ON audit_logs (url, method);