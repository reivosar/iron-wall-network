package event

import (
	"fmt"
)

func Handle(eventID string, event *Event) {
	fmt.Printf("Event ID: %s, Event Data: %v\n", eventID, event)
}
