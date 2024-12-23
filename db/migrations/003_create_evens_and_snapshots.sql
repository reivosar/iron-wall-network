CREATE TABLE events (
    event_id SERIAL PRIMARY KEY,
    aggregate_id VARCHAR(255) NOT NULL,
    aggregate_type VARCHAR(255) NOT NULL,
    event_type VARCHAR(255) NOT NULL,
    event_data JSONB NOT NULL,
    event_triggered_by VARCHAR(255),  
    event_timestamp TIMESTAMPTZ DEFAULT NOW(),
    metadata JSONB
);

CREATE INDEX idx_events_aggregate ON events (aggregate_id, aggregate_type);
CREATE INDEX idx_events_event_type ON events (event_type);
CREATE INDEX idx_events_timestamp ON events (event_timestamp);

CREATE TABLE event_snapshots (
    snapshot_id SERIAL PRIMARY KEY,
    aggregate_id VARCHAR(255) NOT NULL,
    aggregate_type VARCHAR(255) NOT NULL,
    last_event_id INT NOT NULL,
    snapshot_data JSONB NOT NULL,
    snapshot_timestamp TIMESTAMPTZ DEFAULT NOW(),
    FOREIGN KEY (last_event_id) REFERENCES events(event_id) ON DELETE CASCADE
);

CREATE INDEX idx_snapshots_aggregate ON event_snapshots (aggregate_id, aggregate_type);
CREATE INDEX idx_snapshots_timestamp ON event_snapshots (snapshot_timestamp);

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