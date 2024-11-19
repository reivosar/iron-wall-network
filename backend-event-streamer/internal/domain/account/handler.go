package account

import (
	"fmt"
)

func HandleAccountCreated(accountCreatedEvent AccountCreatedEvent) error {
	fmt.Printf("Handling AccountCreatedEvent: %v\n", accountCreatedEvent)
	return nil
}

func HandleAccountApproved(accountApprovedEvent AccountApprovedEvent) error {
	fmt.Printf("Handling AccountApprovedEvent: %v\n", accountApprovedEvent)
	return nil
}

func HandleAccountPended(accountPendedEvent AccountPendedEvent) error {
	fmt.Printf("Handling AccountPendedEvent: %v\n", accountPendedEvent)
	return nil
}

func HandleAccountSuspended(accountSuspendedEvent AccountSuspendedEvent) error {
	fmt.Printf("Handling AccountSuspendedEvent: %v\n", accountSuspendedEvent)
	return nil
}

func HandleAccountActivated(accountActivatedEvent AccountActivatedEvent) error {
	fmt.Printf("Handling AccountActivatedEvent: %v\n", accountActivatedEvent)
	return nil
}

func HandleAccountClosed(accountClosedEvent AccountClosedEvent) error {
	fmt.Printf("Handling AccountClosedEvent: %v\n", accountClosedEvent)
	return nil
}

func HandleFundsDeposited(fundsDepositedEvent FundsDepositedEvent) error {
	fmt.Printf("Handling FundsDepositedEvent: %v\n", fundsDepositedEvent)
	return nil
}

func HandleFundsWithdrawn(fundsWithdrawnEvent FundsWithdrawnEvent) error {
	fmt.Printf("Handling FundsWithdrawnEvent: %v\n", fundsWithdrawnEvent)
	return nil
}

func HandleUserContactInfoUpserted(userContactInfoUpsertedEvent UserContactInfoUpsertedEvent) error {
	fmt.Printf("Handling UserContactInfoUpsertedEvent: %v\n", userContactInfoUpsertedEvent)
	return nil
}

func HandlePhoneNumberUpserted(phoneNumberUpsertedEvent PhoneNumberUpsertedEvent) error {
	fmt.Printf("Handling PhoneNumberUpsertedEvent: %v\n", phoneNumberUpsertedEvent)
	return nil
}

func HandleAddressUpserted(addressUpsertedEvent AddressUpsertedEvent) error {
	return nil
}

func HandleEmergencyContactUpserted(emergencyContactUpsertedEvent EmergencyContactUpsertedEvent) error {
	fmt.Printf("Handling EmergencyContactUpsertedEvent: %v\n", emergencyContactUpsertedEvent)
	return nil
}
