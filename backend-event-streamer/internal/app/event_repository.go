package app

import (
	"backend-event-streamer/internal/infrastructure/db"
	"fmt"
	"strconv"
	"time"
)

type EventRepository struct {
	dc db.DBClient
}

func NewEventRepository(dc db.DBClient) EventRepository {
	return EventRepository{
		dc: dc,
	}
}

func (s *EventRepository) GetEventByID(eventID string) (*DomainEvent, error) {
	event := &DomainEvent{}
	query := `SELECT event_id, aggregate_id, aggregate_type, event_type, event_data, metadata FROM events WHERE event_id = $1`
	err := s.dc.FetchOne(event, query, eventID)
	if err != nil {
		return nil, fmt.Errorf("error fetching event: %v", err)
	}
	return event, nil
}

func (s *EventRepository) MovePendingEventsToActive(eventID string) error {
	return s.dc.WithTransaction(func(tx db.DBTransaction) error {
		eventIDInt, err := strconv.Atoi(eventID)
		if err != nil {
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

		if _, err := tx.ExecuteCommand(query, eventIDInt); err != nil {
			return fmt.Errorf("failed to move event %d to active: %v", eventIDInt, err)
		}
		return nil
	})
}

func (s *EventRepository) RecordProcessedEvent(eventID string) error {
	return s.dc.WithTransaction(func(tx db.DBTransaction) error {
		now := time.Now()

		insertProcessedEventQuery := `INSERT INTO processed_events (event_id) VALUES ($1)`
		if _, err := tx.ExecuteCommand(insertProcessedEventQuery, eventID); err != nil {
			return fmt.Errorf("failed to insert into processed_events: %v", err)
		}

		updateActiveEventQuery := `UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2`
		if _, err := tx.ExecuteCommand(updateActiveEventQuery, now, eventID); err != nil {
			return fmt.Errorf("failed to update active_events: %v", err)
		}
		return nil
	})
}

func (s *EventRepository) RecordFailedEvent(eventID string, failureReason string) error {
	return s.dc.WithTransaction(func(tx db.DBTransaction) error {
		now := time.Now()

		insertFailedEventQuery := `INSERT INTO failed_events (event_id, failure_reason) VALUES ($1, $2)`
		if _, err := tx.ExecuteCommand(insertFailedEventQuery, eventID, failureReason); err != nil {
			return fmt.Errorf("failed to insert into failed_events: %v", err)
		}

		updateActiveEventQuery := `UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2`
		if _, err := tx.ExecuteCommand(updateActiveEventQuery, now, eventID); err != nil {
			return fmt.Errorf("failed to update active_events: %v", err)
		}
		return nil
	})
}
