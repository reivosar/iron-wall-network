package app

import (
	"fmt"
)

func Handle(event *EventStreamData) error {
	if err := MovePendingEventsToActive(event.EventID); err != nil {
		return fmt.Errorf("error moving pending event to active: %v", err)
	}

	domainEvent, err := GetEventByID(event.EventID)
	if err != nil {
		return fmt.Errorf("error fetching event: %v", err)
	}

	err = ProcessStorageEvent(domainEvent)
	if err != nil {
		recordErr := RecordFailedEvent(event.EventID, err.Error())
		if recordErr != nil {
			return fmt.Errorf("error recording failed event: %v", recordErr)
		}
		return fmt.Errorf("error processing storage event: %v", err)
	}

	recordErr := RecordProcessedEvent(event.EventID)
	if recordErr != nil {
		return fmt.Errorf("error recording processed event: %v", recordErr)
	}

	return nil
}
