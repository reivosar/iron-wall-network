package router

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/event/handler"
	"backend-event-streamer/pkg/json"
	"errors"
	"fmt"
	"reflect"
)

type Router struct {
	handler handler.AccountEventHandler
}

func NewAccountEventRouter(handler handler.AccountEventHandler) *Router {
	return &Router{handler: handler}
}

func (r *Router) RouteAccountEvent(eventType string, eventData map[string]interface{}) error {
	switch eventType {
	case "AccountCreated":
		var event event.AccountCreatedEvent
		return r.handleEvent(eventData, &event)
	case "AccountActivated":
		var event event.AccountActivatedEvent
		return r.handleEvent(eventData, &event)
	case "AccountApproved":
		var event event.AccountApprovedEvent
		return r.handleEvent(eventData, &event)
	case "AccountClosed":
		var event event.AccountClosedEvent
		return r.handleEvent(eventData, &event)
	case "AccountPended":
		var event event.AccountPendedEvent
		return r.handleEvent(eventData, &event)
	case "AccountSuspended":
		var event event.AccountSuspendedEvent
		return r.handleEvent(eventData, &event)
	case "AddressUpserted":
		var event event.AddressUpserted
		return r.handleEvent(eventData, &event)
	case "EmergencyContactUpserted":
		var event event.EmergencyContactUpserted
		return r.handleEvent(eventData, &event)
	case "FundsDeposited":
		var event event.FundsDepositedEvent
		return r.handleEvent(eventData, &event)
	case "FundsWithdrawn":
		var event event.FundsWithdrawnEvent
		return r.handleEvent(eventData, &event)
	case "PhoneNumberContactUpserted":
		var event event.PhoneNumberContactUpserted
		return r.handleEvent(eventData, &event)
	case "EmailContactUpserted":
		var event event.EmailContactUpserted
		return r.handleEvent(eventData, &event)
	default:
		return errors.New("unknown event type: " + eventType)
	}
}

func (r *Router) handleEvent(eventData map[string]interface{}, eventStruct interface{}) error {
	_, err := json.ConvertJsonMapToStruct(eventData, eventStruct)
	if err != nil {
		return fmt.Errorf("error handling json mapped event: %v", err)
	}
	switch e := eventStruct.(type) {
	case *event.AccountCreatedEvent:
		return r.handler.HandleAccountCreated(*e)
	case *event.AccountApprovedEvent:
		return r.handler.HandleAccountApproved(*e)
	case *event.AccountClosedEvent:
		return r.handler.HandleAccountClosed(*e)
	case *event.AccountActivatedEvent:
		return r.handler.HandleAccountActivated(*e)
	case *event.AccountPendedEvent:
		return r.handler.HandleAccountPended(*e)
	case *event.AccountSuspendedEvent:
		return r.handler.HandleAccountSuspended(*e)
	case *event.AddressUpserted:
		return r.handler.HandleAddressUpserted(*e)
	case *event.EmergencyContactUpserted:
		return r.handler.HandleEmergencyContactUpserted(*e)
	case *event.FundsDepositedEvent:
		return r.handler.HandleFundsDeposited(*e)
	case *event.FundsWithdrawnEvent:
		return r.handler.HandleFundsWithdrawn(*e)
	case *event.PhoneNumberContactUpserted:
		return r.handler.HandlePhoneNumberContactUpserted(*e)
	case *event.EmailContactUpserted:
		return r.handler.HandleEmailContactUpserted(*e)
	default:
		return fmt.Errorf("unknown event type: %v", reflect.TypeOf(e))
	}
}
