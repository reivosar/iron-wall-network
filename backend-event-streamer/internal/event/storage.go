package event

import (
	"backend-event-streamer/internal/infrastructure/db"
	"context"
	"fmt"
	"strconv"
	"time"
)

func GetEventByID(eventID string) (*DomainEvent, error) {
	db, err := db.NewClient()
	if err != nil {
		return nil, fmt.Errorf("could not connect to database: %w", err)
	}

	var event DomainEvent
	query := `SELECT event_id, aggregate_id, aggregate_type, event_type, event_data, metadata 
	          FROM events WHERE event_id = $1`

	err = db.QueryRow(context.Background(), query, eventID).Scan(
		&event.EventID,
		&event.AggregateID,
		&event.AggregateType,
		&event.EventType,
		&event.EventData,
		&event.Metadata,
	)

	if err != nil {
		return nil, fmt.Errorf("error fetching event: %v", err)
	}

	return &event, nil
}

func MovePendingEventsToActive(eventID string) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	eventIDInt, err := strconv.Atoi(eventID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("invalid event ID format: %v", err)
	}

	query := `
		WITH moved_events AS (
			DELETE FROM pending_events
			WHERE event_id = $1
			RETURNING event_id
		)
		INSERT INTO active_events (event_id)
		SELECT event_id FROM moved_events;
	`

	_, err = tx.Exec(context.Background(), query, eventIDInt)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to move event %d to active: %v", eventIDInt, err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func RecordProcessedEvent(eventID string) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	now := time.Now()

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	insertProcessedEventQuery := `
		INSERT INTO processed_events (event_id) VALUES ($1)
	`
	_, err = tx.Exec(context.Background(), insertProcessedEventQuery, eventID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into processed_events: %v", err)
	}

	updateActiveEventQuery := `
		UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2
	`
	_, err = tx.Exec(context.Background(), updateActiveEventQuery, now, eventID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update active_events: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func RecordFailedEvent(eventID string, failureReason string) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	now := time.Now()

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	insertFailedEventQuery := `
		INSERT INTO failed_events (event_id, failure_reason) VALUES ($1, $2)
	`
	_, err = tx.Exec(context.Background(), insertFailedEventQuery, eventID, failureReason)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into failed_events: %v", err)
	}

	updateActiveEventQuery := `
		UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2
	`
	_, err = tx.Exec(context.Background(), updateActiveEventQuery, now, eventID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update active_events: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}
