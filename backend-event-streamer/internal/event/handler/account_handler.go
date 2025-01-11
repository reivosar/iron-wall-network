package handler

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/infrastructure/db"
	"fmt"
)

type AccountEventHandler interface {
	HandleAccountCreated(event event.AccountCreatedEvent) error
	HandleAccountApproved(event event.AccountApprovedEvent) error
	HandleAccountClosed(event event.AccountClosedEvent) error
	HandleAccountActivated(event event.AccountActivatedEvent) error
	HandleAccountPended(event event.AccountPendedEvent) error
	HandleAccountSuspended(event event.AccountSuspendedEvent) error
	HandleAddressUpserted(event event.AddressUpserted) error
	HandleEmergencyContactUpserted(event event.EmergencyContactUpserted) error
	HandleFundsDeposited(event event.FundsDepositedEvent) error
	HandleFundsWithdrawn(event event.FundsWithdrawnEvent) error
	HandlePhoneNumberContactUpserted(event event.PhoneNumberContactUpserted) error
	HandleEmailContactUpserted(event event.EmailContactUpserted) error
}

type DBAccountEventHandler struct {
	dc db.DBClient
}

func NewAccountEventHandler(dc db.DBClient) AccountEventHandler {
	return &DBAccountEventHandler{dc: dc}
}

func (h *DBAccountEventHandler) HandleAccountCreated(accountCreatedEvent event.AccountCreatedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {

		var userID int
		err := tx.ExecuteQuery(
			&userID, `INSERT INTO bank_users (created_at, updated_at) VALUES (NOW(), NOW()) RETURNING user_id`)
		if err != nil {
			return fmt.Errorf("failed to insert into bank_users: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_user_profiles (user_id, username, full_name, created_at, updated_at) VALUES ($1, $2, $3, NOW(), NOW())`,
			userID, accountCreatedEvent.Username, accountCreatedEvent.FullName); err != nil {
			return fmt.Errorf("failed to insert into bank_user_profiles: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_accounts (account_id, user_id, created_at, updated_at) VALUES ($1, $2, NOW(), NOW())`,
			accountCreatedEvent.AccountID, userID); err != nil {
			return fmt.Errorf("failed to insert into bank_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO pending_accounts (account_id, pended_at) VALUES ($1, NOW())`,
			accountCreatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into pending_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_account_status (account_id, status, created_at, updated_at) VALUES ($1, 'Pending', NOW(), NOW())`,
			accountCreatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into bank_account_status: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAccountApproved(accountApprovedEvent event.AccountApprovedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`INSERT INTO approved_accounts (account_id, approved_at) VALUES ($1, NOW())`,
			accountApprovedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into approved_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`UPDATE bank_account_status SET status = 'Approved', updated_at = NOW() WHERE account_id = $1`,
			accountApprovedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_account_status: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAccountPended(accountPendedEvent event.AccountPendedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`INSERT INTO pending_accounts (account_id, pended_at) VALUES ($1, NOW())`,
			accountPendedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into pending_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`UPDATE bank_account_status SET status = 'Pending', updated_at = NOW() WHERE account_id = $1`,
			accountPendedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_account_status: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAccountSuspended(accountSuspendedEvent event.AccountSuspendedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`INSERT INTO suspend_accounts (account_id, suspended_at, suspension_reason) VALUES ($1, NOW(), $2)`,
			accountSuspendedEvent.AccountID, accountSuspendedEvent.Reason); err != nil {
			return fmt.Errorf("failed to insert into suspend_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`UPDATE bank_account_status SET status = 'Suspended', updated_at = NOW() WHERE account_id = $1`,
			accountSuspendedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_account_status: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAccountActivated(accountActivatedEvent event.AccountActivatedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`DELETE FROM pending_accounts WHERE account_id = $1`,
			accountActivatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to delete from pending_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO active_accounts (account_id, activated_at) VALUES ($1, NOW())`,
			accountActivatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into active_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`UPDATE bank_account_status SET status = 'Active', updated_at = NOW() WHERE account_id = $1`,
			accountActivatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_account_status: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_balance (account_id, balance, balance_timestamp) VALUES ($1, 0, NOW())`,
			accountActivatedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to insert into bank_balance: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_account_credentials (account_id, password_hash, expires_at, created_at, updated_at)
			VALUES ($1, $2, NOW() + INTERVAL '1 month', NOW(), NOW())`,
			accountActivatedEvent.AccountID, accountActivatedEvent.Password); err != nil {
			return fmt.Errorf("failed to insert into bank_account_credentials: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAccountClosed(accountClosedEvent event.AccountClosedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`INSERT INTO closed_accounts (account_id, closed_at, closure_reason) VALUES ($1, NOW(), $2)`,
			accountClosedEvent.AccountID, accountClosedEvent.Reason); err != nil {
			return fmt.Errorf("failed to insert into closed_accounts: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`UPDATE bank_account_status SET status = 'Closed', updated_at = NOW() WHERE account_id = $1`,
			accountClosedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_account_status: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleFundsDeposited(fundsDepositedEvent event.FundsDepositedEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`UPDATE bank_balance SET balance = $1, balance_timestamp = NOW() WHERE account_id = $2`,
			fundsDepositedEvent.TotalBalance, fundsDepositedEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_balance: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp) VALUES ($1,'deposit',$2, NOW())`,
			fundsDepositedEvent.AccountID, fundsDepositedEvent.Amount); err != nil {
			return fmt.Errorf("failed to insert into bank_balance_history: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleFundsWithdrawn(fundsWithdrawnEvent event.FundsWithdrawnEvent) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		if _, err := tx.ExecuteCommand(
			`UPDATE bank_balance SET balance = $1, balance_timestamp = NOW() WHERE account_id = $2`,
			fundsWithdrawnEvent.TotalBalance, fundsWithdrawnEvent.AccountID); err != nil {
			return fmt.Errorf("failed to update bank_balance: %w", err)
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp) VALUES ($1, 'withdrawal', $2, NOW())`,
			fundsWithdrawnEvent.AccountID, -fundsWithdrawnEvent.Amount); err != nil {
			return fmt.Errorf("failed to insert into bank_balance_history: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleEmailContactUpserted(userContactInfoUpsertedEvent event.EmailContactUpserted) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		userID, err := h.findUserIdByAccountId(tx, userContactInfoUpsertedEvent.AccountID)
		if err != nil {
			return err
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_user_email_contacts (user_id, email, created_at, updated_at) VALUES ($1, $2, NOW(), NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET email = $2, updated_at = NOW()`,
			userID, userContactInfoUpsertedEvent.Email); err != nil {
			return fmt.Errorf("failed to upsert user contact info: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandlePhoneNumberContactUpserted(phoneNumberUpsertedEvent event.PhoneNumberContactUpserted) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		userID, err := h.findUserIdByAccountId(tx, phoneNumberUpsertedEvent.AccountID)
		if err != nil {
			return err
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_user_phone_number_contacts (user_id, phone_number, type, updated_at)
			VALUES ($1, $2, $3, NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				phone_number = EXCLUDED.phone_number, 
				type = EXCLUDED.type, 
				updated_at = NOW()`,
			userID, phoneNumberUpsertedEvent.PhoneNumber, phoneNumberUpsertedEvent.PhoneType); err != nil {
			return fmt.Errorf("failed to upsert phone number: %w", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) HandleAddressUpserted(addressUpsertedEvent event.AddressUpserted) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		userID, err := h.findUserIdByAccountId(tx, addressUpsertedEvent.AccountID)
		if err != nil {
			return err
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_user_addresses 
				(user_id, postal_code, prefecture, city, town_area, building_name, address_type, created_at, updated_at)
			VALUES 
				($1, $2, $3, $4, $5, $6, $7, NOW(), $8) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				building_name = $6, 
				address_type = $7,
				updated_at = $8`,
			userID,
			addressUpsertedEvent.PostalCode,
			addressUpsertedEvent.Prefecture,
			addressUpsertedEvent.City,
			addressUpsertedEvent.TownArea,
			addressUpsertedEvent.BuildingName,
			addressUpsertedEvent.AddressType,
			addressUpsertedEvent.UpdatedAt); err != nil {
			return fmt.Errorf("failed to upsert addresses: %w", err)
		}

		return nil
	})

}

func (h *DBAccountEventHandler) HandleEmergencyContactUpserted(emergencyContactUpsertedEvent event.EmergencyContactUpserted) error {
	return h.dc.WithTransaction(func(tx db.DBTransaction) error {
		userID, err := h.findUserIdByAccountId(tx, emergencyContactUpsertedEvent.AccountID)
		if err != nil {
			return err
		}

		if _, err := tx.ExecuteCommand(
			`INSERT INTO bank_user_emergency_contacts (user_id, contact_name, contact_phone, created_at, updated_at) VALUES ($1, $2, $3, NOW(), NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				contact_name = EXCLUDED.contact_name,
				contact_phone = EXCLUDED.contact_phone,
				updated_at = NOW()`,
			userID,
			emergencyContactUpsertedEvent.ContactName,
			emergencyContactUpsertedEvent.ContactPhone); err != nil {
			return fmt.Errorf("failed to upsert emergency contact: %v", err)
		}

		return nil
	})
}

func (h *DBAccountEventHandler) findUserIdByAccountId(tx db.DBTransaction, accountId string) (int, error) {
	result, err := tx.ExecuteQueryRowAsMap(`SELECT user_id FROM bank_accounts WHERE account_id = $1`, accountId)
	if err != nil {
		return -1, fmt.Errorf("failed to execute query for account_id %v: %w", accountId, err)
	}
	if result.IsEmpty() {
		return -1, fmt.Errorf("no user found for account_id %v: %w", accountId, err)
	}
	return result.GetInt("user_id")
}
