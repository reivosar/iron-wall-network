package account

import (
	jsonmapper "backend-event-streamer/internal/shared"
	"fmt"
)

type AccountCreatedEvent struct {
	Email     string `json:"email"`
	FullName  string `json:"fullName"`
	Username  string `json:"username"`
	AccountID string `json:"accountId"`
	CreatedAt string `json:"createdAt"`
}

func HandleAccountCreated(eventData map[string]interface{}) error {
	var accountCreatedEvent AccountCreatedEvent

	if err := jsonmapper.JsonMapToStruct(eventData, &accountCreatedEvent); err != nil {
		return fmt.Errorf("error handling account created event: %v", err)
	}

	return nil
}
