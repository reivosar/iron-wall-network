package app

import (
	"encoding/json"
	"fmt"
)

type EventStreamData struct {
	EventID       string `json:"eventId"`
	AggregateID   string `json:"aggregateId"`
	AggregateType string `json:"aggregateType"`
	EventType     string `json:"eventType"`
}

type DomainEvent struct {
	EventID       int     `json:"event_id" db:"event_id"`
	AggregateID   string  `json:"aggregate_id" db:"aggregate_id"`
	AggregateType string  `json:"aggregate_type" db:"aggregate_type"`
	EventType     string  `json:"event_type" db:"event_type"`
	EventData     string  `json:"event_data" db:"event_data"`
	Metadata      *string `json:"metadata" db:"metadata"`
}

func ToEventStreamData(messageValues map[string]interface{}) (*EventStreamData, error) {
	messageJSON, err := json.Marshal(messageValues)
	if err != nil {
		return nil, fmt.Errorf("error marshalling message %v", err)
	}

	var event EventStreamData
	err = json.Unmarshal(messageJSON, &event)
	if err != nil {
		return nil, err
	}

	if event.EventID == "" || event.AggregateID == "" || event.AggregateType == "" || event.EventType == "" {
		return nil, fmt.Errorf("missing required fields in event data")
	}

	return &event, nil
}
