package event

import (
	"fmt"
)

func Handle(event *StreamEvent) {
	if err := MovePendingEventsToActive(event.EventID); err != nil {
		fmt.Println("Error moving pending event to active:", err)
		return
	}

	storageEvent, err := GetEventByID(event.EventID)
	if err != nil {
		fmt.Println("Error fetching event:", err)
		return
	}

	err = ProcessStorageEvent(storageEvent)
	if err != nil {
		recordErr := RecordFailedEvent(event.EventID, err.Error())
		if recordErr != nil {
			fmt.Println("Error recording failed event:", recordErr)
		}
		return
	}

	recordErr := RecordProcessedEvent(event.EventID)
	if recordErr != nil {
		fmt.Println("Error recording processed event:", recordErr)
	}
}
