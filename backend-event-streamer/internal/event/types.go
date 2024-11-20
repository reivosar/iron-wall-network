package event

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
	EventID       int    `json:"event_id"`
	AggregateID   string `json:"aggregate_id"`
	AggregateType string `json:"aggregate_type"`
	EventType     string `json:"event_type"`
	EventData     string `json:"event_data"`
	Metadata      string `json:"metadata"`
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

	return &event, nil
}
