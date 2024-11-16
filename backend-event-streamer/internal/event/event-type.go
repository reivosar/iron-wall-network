package event

import (
	"encoding/json"
	"fmt"
	"log"
)

type Event struct {
	EventID       string `json:"eventId"`
	AggregateID   string `json:"aggregateId"`
	AggregateType string `json:"aggregateType"`
	EventType     string `json:"eventType"`
}

func ParseMessage(eventID string, messageValues map[string]interface{}) (*Event, error) {
	messageJSON, err := json.Marshal(messageValues)
	if err != nil {
		log.Printf("Error marshalling message for Event ID: %s: %v", eventID, err)
		return nil, fmt.Errorf("error marshalling message for Event ID: %s: %v", eventID, err)
	}

	var event Event
	err = json.Unmarshal(messageJSON, &event)
	if err != nil {
		log.Printf("Error unmarshalling message for Event ID: %s: %v", eventID, err)
		return nil, err
	}

	return &event, nil
}
