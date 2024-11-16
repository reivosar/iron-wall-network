package event

import (
	"encoding/json"
	"fmt"
)

type StreamEvent struct {
	EventID       string `json:"eventId"`
	AggregateID   string `json:"aggregateId"`
	AggregateType string `json:"aggregateType"`
	EventType     string `json:"eventType"`
}

func ToEvent(messageValues map[string]interface{}) (*StreamEvent, error) {
	messageJSON, err := json.Marshal(messageValues)
	if err != nil {
		return nil, fmt.Errorf("error marshalling message %v", err)
	}

	var event StreamEvent
	err = json.Unmarshal(messageJSON, &event)
	if err != nil {
		return nil, err
	}

	return &event, nil
}
