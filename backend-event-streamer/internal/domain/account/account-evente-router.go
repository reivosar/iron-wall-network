package account

import "errors"

func RouteAccountEvent(eventType string, eventData map[string]interface{}) error {
	switch eventType {
	case "AccountActivated":
		return HandleAccountActivated(eventData)
	case "AccountApproved":
		return HandleAccountApproved(eventData)
	case "AccountClosed":
		return HandleAccountClosed(eventData)
	case "AccountCreated":
		return HandleAccountCreated(eventData)
	case "AccountPended":
		return HandleAccountPended(eventData)
	case "AccountSuspended":
		return HandleAccountSuspended(eventData)
	case "AddressUpserted":
		return HandleAddressUpserted(eventData)
	case "EmergencyContactUpserted":
		return HandleEmergencyContactUpserted(eventData)
	case "FundsDeposited":
		return HandleFundsDeposited(eventData)
	case "FundsWithdrawn":
		return HandleFundsWithdrawn(eventData)
	case "PhoneNumberUpserted":
		return HandlePhoneNumberUpserted(eventData)
	case "UserContactInfoUpserted":
		return HandleUserContactInfoUpserted(eventData)
	default:
		return errors.New("unknown event type: " + eventType)
	}
}
