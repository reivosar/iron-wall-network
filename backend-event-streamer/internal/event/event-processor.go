package event

import (
	"backend-event-streamer/internal/domain/account"
	"encoding/json"
	"errors"
	"fmt"
)

func ProcessStorageEvent(storageEvent *Event) error {
	var eventData map[string]interface{}
	err := json.Unmarshal([]byte(storageEvent.EventData), &eventData)
	if err != nil {
		return fmt.Errorf("error unmarshalling event data for Event ID: %v, Error: %v", storageEvent.EventID, err)
	}

	switch storageEvent.AggregateType {
	case "account":
		return account.RouteAccountEvent(storageEvent.EventType, eventData)
	}
	return errors.New("unknown aggregate type: " + storageEvent.AggregateType)
}
