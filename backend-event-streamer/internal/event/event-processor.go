package event

import (
	"backend-event-streamer/internal/domain/account"
	"backend-event-streamer/internal/storage"
	"encoding/json"
	"errors"
	"fmt"
)

func ProcessStorageEvent(storageEvent *storage.Event) error {
	var eventData map[string]interface{}
	err := json.Unmarshal([]byte(storageEvent.EventData), &eventData)
	if err != nil {
		return fmt.Errorf("error unmarshalling event data for Event ID: %s, Error: %v", storageEvent.EventID, err)
	}

	switch storageEvent.AggregateType {
	case "account":
		return processStorageAccountEvent(storageEvent.EventType, eventData)
	}
	return errors.New("unknown aggregate type: " + storageEvent.AggregateType)
}

func processStorageAccountEvent(eventType string, eventData map[string]interface{}) error {
	switch eventType {
	case "AccountActivated":
		return account.HandleAccountActivated(eventData)
	case "AccountApproved":
		return account.HandleAccountApproved(eventData)
	case "AccountClosed":
		return account.HandleAccountClosed(eventData)
	case "AccountCreated":
		return account.HandleAccountCreated(eventData)
	case "AccountPending":
		return account.HandleAccountPending(eventData)
	case "AccountSuspended":
		return account.HandleAccountSuspended(eventData)
	case "AddressUpserted":
		return account.HandleAddressUpserted(eventData)
	case "EmergencyContactUpserted":
		return account.HandleEmergencyContactUpserted(eventData)
	case "FundsDeposited":
		return account.HandleFundsDeposited(eventData)
	case "FundsWithdrawn":
		return account.HandleFundsWithdrawn(eventData)
	case "PhoneNumberUpserted":
		return account.HandlePhoneNumberUpserted(eventData)
	case "UserContactInfoUpserted":
		return account.HandleUserContactInfoUpserted(eventData)
	default:
		return errors.New("unknown event type: " + eventType)
	}
}
