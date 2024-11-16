package event

import (
	"backend-event-streamer/internal/storage"
	"fmt"
)

func Handle(event *StreamEvent) {
	if err := storage.MovePendingEventsToActive(event.EventID); err != nil {
		fmt.Println("Error moving pending event to active:", err)
		return
	}

	storageEvent, err := storage.GetEventByID(event.EventID)
	if err != nil {
		fmt.Println("Error fetching event:", err)
		return
	}

	err = ProcessStorageEvent(storageEvent)
	if err != nil {
		recordErr := storage.RecordFailedEvent(event.EventID, err.Error())
		if recordErr != nil {
			fmt.Println("Error recording failed event:", recordErr)
		}
		return
	}

	recordErr := storage.RecordProcessedEvent(event.EventID)
	if recordErr != nil {
		fmt.Println("Error recording processed event:", recordErr)
	}
}
