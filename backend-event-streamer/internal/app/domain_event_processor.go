package app

import (
	"backend-event-streamer/internal/event/handler"
	"backend-event-streamer/internal/event/router"
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
		account_router := router.NewAccountEventRouter(handler.NewHandler())
		return account_router.RouteAccountEvent(domainEvent.EventType, eventData)
	}
	return errors.New("unknown aggregate type: " + domainEvent.AggregateType)
}
