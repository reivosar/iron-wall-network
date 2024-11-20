package account

import (
	"backend-event-streamer/pkg/json"
	"errors"
	"fmt"
	"reflect"
)

func RouteAccountEvent(eventType string, eventData map[string]interface{}) error {
	switch eventType {
	case "AccountCreated":
		var event AccountCreatedEvent
		return handleEvent(eventData, &event)
	case "AccountActivated":
		var event AccountActivatedEvent
		return handleEvent(eventData, &event)
	case "AccountApproved":
		var event AccountApprovedEvent
		return handleEvent(eventData, &event)
	case "AccountClosed":
		var event AccountClosedEvent
		return handleEvent(eventData, &event)
	case "AccountPended":
		var event AccountPendedEvent
		return handleEvent(eventData, &event)
	case "AccountSuspended":
		var event AccountSuspendedEvent
		return handleEvent(eventData, &event)
	case "AddressUpserted":
		var event AddressUpsertedEvent
		return handleEvent(eventData, &event)
	case "EmergencyContactUpserted":
		var event EmergencyContactUpsertedEvent
		return handleEvent(eventData, &event)
	case "FundsDeposited":
		var event FundsDepositedEvent
		return handleEvent(eventData, &event)
	case "FundsWithdrawn":
		var event FundsWithdrawnEvent
		return handleEvent(eventData, &event)
	case "PhoneNumberUpserted":
		var event PhoneNumberUpsertedEvent
		return handleEvent(eventData, &event)
	case "UserContactInfoUpserted":
		var event UserContactInfoUpsertedEvent
		return handleEvent(eventData, &event)
	default:
		return errors.New("unknown event type: " + eventType)
	}
}

func handleEvent(eventData map[string]interface{}, eventStruct interface{}) error {
	_, err := json.ConvertJsonMapToStruct(eventData, eventStruct)
	if err != nil {
		return fmt.Errorf("error handling json mapped event: %v", err)
	}
	switch e := eventStruct.(type) {
	case *AccountCreatedEvent:
		return HandleAccountCreated(*e)
	case *AccountApprovedEvent:
		return HandleAccountApproved(*e)
	case *AccountClosedEvent:
		return HandleAccountClosed(*e)
	case *AccountActivatedEvent:
		return HandleAccountActivated(*e)
	case *AccountPendedEvent:
		return HandleAccountPended(*e)
	case *AccountSuspendedEvent:
		return HandleAccountSuspended(*e)
	case *AddressUpsertedEvent:
		return HandleAddressUpserted(*e)
	case *EmergencyContactUpsertedEvent:
		return HandleEmergencyContactUpserted(*e)
	case *FundsDepositedEvent:
		return HandleFundsDeposited(*e)
	case *FundsWithdrawnEvent:
		return HandleFundsWithdrawn(*e)
	case *PhoneNumberUpsertedEvent:
		return HandlePhoneNumberUpserted(*e)
	case *UserContactInfoUpsertedEvent:
		return HandleUserContactInfoUpserted(*e)
	default:
		return fmt.Errorf("unknown event type: %v", reflect.TypeOf(e))
	}
}
