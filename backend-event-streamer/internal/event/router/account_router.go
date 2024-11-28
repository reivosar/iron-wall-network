package router

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/event/handler"
	"backend-event-streamer/pkg/json"
	"errors"
	"fmt"
	"reflect"
)

func RouteAccountEvent(eventType string, eventData map[string]interface{}) error {
	switch eventType {
	case "AccountCreated":
		var event event.AccountCreatedEvent
		return handleEvent(eventData, &event)
	case "AccountActivated":
		var event event.AccountActivatedEvent
		return handleEvent(eventData, &event)
	case "AccountApproved":
		var event event.AccountApprovedEvent
		return handleEvent(eventData, &event)
	case "AccountClosed":
		var event event.AccountClosedEvent
		return handleEvent(eventData, &event)
	case "AccountPended":
		var event event.AccountPendedEvent
		return handleEvent(eventData, &event)
	case "AccountSuspended":
		var event event.AccountSuspendedEvent
		return handleEvent(eventData, &event)
	case "AddressUpserted":
		var event event.AddressUpsertedEvent
		return handleEvent(eventData, &event)
	case "EmergencyContactUpserted":
		var event event.EmergencyContactUpsertedEvent
		return handleEvent(eventData, &event)
	case "FundsDeposited":
		var event event.FundsDepositedEvent
		return handleEvent(eventData, &event)
	case "FundsWithdrawn":
		var event event.FundsWithdrawnEvent
		return handleEvent(eventData, &event)
	case "PhoneNumberUpserted":
		var event event.PhoneNumberUpsertedEvent
		return handleEvent(eventData, &event)
	case "UserContactInfoUpserted":
		var event event.UserContactInfoUpsertedEvent
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
	case *event.AccountCreatedEvent:
		return handler.HandleAccountCreated(*e)
	case *event.AccountApprovedEvent:
		return handler.HandleAccountApproved(*e)
	case *event.AccountClosedEvent:
		return handler.HandleAccountClosed(*e)
	case *event.AccountActivatedEvent:
		return handler.HandleAccountActivated(*e)
	case *event.AccountPendedEvent:
		return handler.HandleAccountPended(*e)
	case *event.AccountSuspendedEvent:
		return handler.HandleAccountSuspended(*e)
	case *event.AddressUpsertedEvent:
		return handler.HandleAddressUpserted(*e)
	case *event.EmergencyContactUpsertedEvent:
		return handler.HandleEmergencyContactUpserted(*e)
	case *event.FundsDepositedEvent:
		return handler.HandleFundsDeposited(*e)
	case *event.FundsWithdrawnEvent:
		return handler.HandleFundsWithdrawn(*e)
	case *event.PhoneNumberUpsertedEvent:
		return handler.HandlePhoneNumberUpserted(*e)
	case *event.UserContactInfoUpsertedEvent:
		return handler.HandleUserContactInfoUpserted(*e)
	default:
		return fmt.Errorf("unknown event type: %v", reflect.TypeOf(e))
	}
}
