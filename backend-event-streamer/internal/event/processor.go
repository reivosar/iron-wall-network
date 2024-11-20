package event

import (
	"backend-event-streamer/internal/domain/account"
	"encoding/json"
	"errors"
	"fmt"
)

func ProcessStorageEvent(domainEvent *DomainEvent) error {
	var eventData map[string]interface{}
	err := json.Unmarshal([]byte(domainEvent.EventData), &eventData)
	if err != nil {
		return fmt.Errorf("error unmarshalling event data for Event ID: %v, Error: %v", domainEvent.EventID, err)
	}

	switch domainEvent.AggregateType {
	case "account":
		return account.RouteAccountEvent(domainEvent.EventType, eventData)
	}
	return errors.New("unknown aggregate type: " + domainEvent.AggregateType)
}
