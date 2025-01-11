package app

import (
	"backend-event-streamer/internal/infrastructure/db"
	"backend-event-streamer/mocks"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestGetEventByID(t *testing.T) {
	t.Run("should fetch event by ID", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		service := &EventRepository{dc: mockDBClient}

		query := `SELECT event_id, aggregate_id, aggregate_type, event_type, event_data, metadata FROM events WHERE event_id = $1`

		// Expected values for the event
		expectedEventID := 1
		expectedAggregateID := "AggregateID"
		expectedAggregateType := "AggregateType"
		expectedEventType := "EventType"
		expectedEventData := "{}"
		expectedMetadata := "TestMetadata"

		var returnEvent = &DomainEvent{
			EventID:       expectedEventID,
			AggregateID:   expectedAggregateID,
			AggregateType: expectedAggregateType,
			EventType:     expectedEventType,
			EventData:     expectedEventData,
			Metadata:      &expectedMetadata,
		}

		// Mock the return value for FetchOne
		mockDBClient.On("FetchOne",
			mock.AnythingOfType("*app.DomainEvent"),
			query,
			"1",
		).Run(func(args mock.Arguments) {
			*(args[0].(*DomainEvent)) = *returnEvent
		}).Return(nil).Once()

		// WHEN
		event, err := service.GetEventByID("1")

		// THEN
		assert.NoError(t, err, "Expected no error, but got one")
		assert.NotNil(t, event, "Expected event to be not nil")
		assert.Equal(t, expectedEventID, event.EventID)
		assert.Equal(t, expectedAggregateID, event.AggregateID)
		assert.Equal(t, expectedAggregateType, event.AggregateType)
		assert.Equal(t, expectedEventType, event.EventType)
		assert.Equal(t, expectedEventData, event.EventData)
		assert.Equal(t, &expectedMetadata, event.Metadata)

		mockDBClient.AssertExpectations(t)
	})
}

func TestMovePendingEventsToActive(t *testing.T) {
	t.Run("should move pending events to active", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		service := NewEventRepository(mockDBClient)

		query := `
			WITH moved_events AS (
				DELETE FROM pending_events
				WHERE event_id = $1
				RETURNING event_id
			)
			INSERT INTO active_events (event_id)
			SELECT event_id FROM moved_events;
		`

		mockDBClient.On("WithTransaction", mock.Anything).Run(func(args mock.Arguments) {
			action := args.Get(0).(func(db.DBTransaction) error)
			action(mockTx)
		}).Return(nil)

		mockTx.On("ExecuteCommand", query, 1).Return(1, nil)

		// WHEN
		err := service.MovePendingEventsToActive("1")

		// THEN
		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestRecordProcessedEvent(t *testing.T) {
	t.Run("should record processed event", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		service := NewEventRepository(mockDBClient)

		insertQuery := `INSERT INTO processed_events (event_id) VALUES ($1)`
		updateQuery := `UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2`

		mockDBClient.On("WithTransaction", mock.Anything).Run(func(args mock.Arguments) {
			action := args.Get(0).(func(db.DBTransaction) error)
			action(mockTx)
		}).Return(nil)

		mockTx.On("ExecuteCommand", insertQuery, "1").Return(1, nil)
		mockTx.On("ExecuteCommand", updateQuery, mock.Anything, "1").Return(1, nil)

		// WHEN
		err := service.RecordProcessedEvent("1")

		// THEN
		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestRecordFailedEvent(t *testing.T) {
	t.Run("should record failed event", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		service := NewEventRepository(mockDBClient)

		insertQuery := `INSERT INTO failed_events (event_id, failure_reason) VALUES ($1, $2)`
		updateQuery := `UPDATE active_events SET processing_finished_at = $1 WHERE event_id = $2`

		mockDBClient.On("WithTransaction", mock.Anything).Run(func(args mock.Arguments) {
			action := args.Get(0).(func(db.DBTransaction) error)
			action(mockTx)
		}).Return(nil)

		mockTx.On("ExecuteCommand", insertQuery, "1", "failure reason").Return(1, nil)
		mockTx.On("ExecuteCommand", updateQuery, mock.Anything, "1").Return(1, nil)

		// WHEN
		err := service.RecordFailedEvent("1", "failure reason")

		// THEN
		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}
