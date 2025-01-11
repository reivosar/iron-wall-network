package app

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestToEventStreamData(t *testing.T) {
	t.Run("should correctly convert valid message values to EventStreamData", func(t *testing.T) {
		// GIVEN
		messageValues := map[string]interface{}{
			"eventId":       "12345",
			"aggregateId":   "67890",
			"aggregateType": "Account",
			"eventType":     "AccountCreated",
		}

		// WHEN
		eventStreamData, err := ToEventStreamData(messageValues)

		// THEN
		assert.NoError(t, err)
		assert.NotNil(t, eventStreamData)
		assert.Equal(t, "12345", eventStreamData.EventID)
		assert.Equal(t, "67890", eventStreamData.AggregateID)
		assert.Equal(t, "Account", eventStreamData.AggregateType)
		assert.Equal(t, "AccountCreated", eventStreamData.EventType)
	})

	t.Run("should return an error for invalid message values", func(t *testing.T) {
		// GIVEN
		messageValues := map[string]interface{}{
			"eventId":       12345, // Invalid type, should be string
			"aggregateId":   "67890",
			"aggregateType": "Account",
			"eventType":     "AccountCreated",
		}

		// WHEN
		eventStreamData, err := ToEventStreamData(messageValues)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, eventStreamData)
	})

	t.Run("should return an error for missing required fields", func(t *testing.T) {
		// GIVEN
		messageValues := map[string]interface{}{
			"aggregateId":   "67890",
			"aggregateType": "Account",
			"eventType":     "AccountCreated",
		}

		// WHEN
		eventStreamData, err := ToEventStreamData(messageValues)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, eventStreamData)
	})

	t.Run("should return an error if messageValues cannot be marshalled", func(t *testing.T) {
		// GIVEN
		messageValues := map[string]interface{}{
			"invalidField": make(chan int), // Invalid type that cannot be marshalled to JSON
		}

		// WHEN
		eventStreamData, err := ToEventStreamData(messageValues)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, eventStreamData)
	})
}
