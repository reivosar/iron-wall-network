CREATE TABLE events (
    event_id SERIAL PRIMARY KEY,
    aggregate_id VARCHAR(255) NOT NULL,
    aggregate_type VARCHAR(255) NOT NULL,
    event_type VARCHAR(255) NOT NULL,
    event_data JSONB NOT NULL,
    sequence_number BIGINT NOT NULL,
    version BIGINT NOT NULL,
    triggered_by VARCHAR(255) DEFAULT NULL,
    occurred_at TIMESTAMPTZ DEFAULT NOW(),
    metadata JSONB,
    UNIQUE (aggregate_id, aggregate_type, sequence_number)
);

CREATE TABLE latest_event_pointers (
    pointer_id SERIAL PRIMARY KEY,
    aggregate_id VARCHAR(255) NOT NULL,
    aggregate_type VARCHAR(255) NOT NULL,
    event_type VARCHAR(255) NOT NULL,
    last_event_id BIGINT NOT NULL,
    last_sequence_number BIGINT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW() ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (last_event_id) REFERENCES events(event_id) ON DELETE CASCADE,
    UNIQUE (aggregate_id, aggregate_type, event_type)
);

CREATE INDEX idx_events_aggregate_id_type_sequence_number ON events (aggregate_id, aggregate_type, sequence_number);
CREATE INDEX idx_occurred_at ON events (occurred_at);

CREATE INDEX idx_latest_event_aggregate ON latest_event_pointers (aggregate_id, aggregate_type);

CREATE TABLE pending_events (
    event_id INT NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    pended_at TIMESTAMPTZ DEFAULT NOW(),
    PRIMARY KEY (event_id)
);

CREATE TABLE active_events (
    event_id INT NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    processing_started_at TIMESTAMPTZ DEFAULT NOW(),
    processing_finished_at TIMESTAMPTZ,
    PRIMARY KEY (event_id)
);

CREATE TABLE processed_events (
    event_id INT NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    processed_at TIMESTAMPTZ DEFAULT NOW(),
    PRIMARY KEY (event_id)
);

CREATE TABLE failed_events (
    event_id INT NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    failed_at TIMESTAMPTZ DEFAULT NOW(),
    failure_reason TEXT,
    retry_count INT DEFAULT 0,
    PRIMARY KEY (event_id)
);