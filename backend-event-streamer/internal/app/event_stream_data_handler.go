package app

import (
	"fmt"
)

type EventStreamHandler struct {
	er EventRepository
}

func NewEventStreamHandler(er EventRepository) EventStreamHandler {
	return EventStreamHandler{er: er}
}

func (h *EventStreamHandler) Handle(event *EventStreamData) error {
	if err := h.er.MovePendingEventsToActive(event.EventID); err != nil {
		return fmt.Errorf("error moving pending event to active: %v", err)
	}

	domainEvent, err := h.er.GetEventByID(event.EventID)
	if err != nil {
		return fmt.Errorf("error fetching event: %v", err)
	}

	err = ProcessStorageEvent(domainEvent)
	if err != nil {
		recordErr := h.er.RecordFailedEvent(event.EventID, err.Error())
		if recordErr != nil {
			return fmt.Errorf("error recording failed event: %v", recordErr)
		}
		return fmt.Errorf("error processing storage event: %v", err)
	}

	recordErr := h.er.RecordProcessedEvent(event.EventID)
	if recordErr != nil {
		return fmt.Errorf("error recording processed event: %v", recordErr)
	}

	return nil
}
