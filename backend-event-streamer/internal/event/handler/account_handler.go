package handler

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/infrastructure/db"
	"context"
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

type Handler struct {
}

func NewHandler() AccountEventHandler {
	return &Handler{}
}

func (h *Handler) HandleAccountCreated(accountCreatedEvent event.AccountCreatedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	var userID int
	err = tx.QueryRow(context.Background(), `
		INSERT INTO bank_users (created_at, updated_at)
		VALUES (NOW(), NOW())
		RETURNING user_id`).Scan(&userID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_users: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_profiles (user_id, username, full_name, created_at, updated_at)
		VALUES ($1, $2, $3, NOW(), NOW())`, userID, accountCreatedEvent.Username, accountCreatedEvent.FullName)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_user_profiles: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_accounts (account_id, user_id, created_at, updated_at)
		VALUES ($1, $2, NOW(), NOW())`, accountCreatedEvent.AccountID, userID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO pending_accounts (account_id, pended_at)
		VALUES ($1, NOW())`, accountCreatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into pending_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        INSERT INTO bank_account_status (account_id, status, created_at, updated_at)
        VALUES ($1, 'Pending', NOW(), NOW())`, accountCreatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_account_status: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAccountApproved(accountApprovedEvent event.AccountApprovedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO approved_accounts (account_id, approved_at)
		VALUES ($1, NOW())`, accountApprovedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into approved_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        UPDATE bank_account_status 
        SET status = 'Approved', updated_at = NOW()
        WHERE account_id = $1`, accountApprovedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_account_status: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAccountPended(accountPendedEvent event.AccountPendedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO pending_accounts (account_id, pended_at)
		VALUES ($1, NOW())`, accountPendedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into pending_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        UPDATE bank_account_status 
        SET status = 'Pending', updated_at = NOW()
        WHERE account_id = $1`, accountPendedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_account_status: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAccountSuspended(accountSuspendedEvent event.AccountSuspendedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO suspend_accounts (account_id, suspended_at, suspension_reason)
		VALUES ($1, NOW(), $2)`, accountSuspendedEvent.AccountID, accountSuspendedEvent.Reason)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into suspend_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        UPDATE bank_account_status 
        SET status = 'Suspended', updated_at = NOW()
        WHERE account_id = $1`, accountSuspendedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_account_status: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAccountActivated(accountActivatedEvent event.AccountActivatedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		DELETE FROM pending_accounts WHERE account_id = $1`, accountActivatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to delete from pending_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO active_accounts (account_id, activated_at)
		VALUES ($1, NOW())`, accountActivatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into active_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        UPDATE bank_account_status 
        SET status = 'Active', updated_at = NOW()
        WHERE account_id = $1`, accountActivatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_account_status: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        INSERT INTO bank_balance (account_id, balance, balance_timestamp)
        VALUES ($1, 0, NOW())`, accountActivatedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_balance: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        INSERT INTO bank_account_credentials (account_id, password_hash, expires_at, created_at, updated_at)
        VALUES ($1, $2, NOW() + INTERVAL '1 month', NOW(), NOW())`,
		accountActivatedEvent.AccountID, accountActivatedEvent.Password)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_account_credentials: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAccountClosed(accountClosedEvent event.AccountClosedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO closed_accounts (account_id, closed_at, closure_reason)
		VALUES ($1, NOW(), $2)`, accountClosedEvent.AccountID, accountClosedEvent.Reason)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into closed_accounts: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
        UPDATE bank_account_status 
        SET status = 'Closed', updated_at = NOW()
        WHERE account_id = $1`, accountClosedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_account_status: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleFundsDeposited(fundsDepositedEvent event.FundsDepositedEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		UPDATE bank_balance 
		SET balance = $1, balance_timestamp = NOW() 
		WHERE account_id = $2`, fundsDepositedEvent.TotalBalance, fundsDepositedEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_balance: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp)
		VALUES ($1,'deposit',$2, NOW())`, fundsDepositedEvent.AccountID, fundsDepositedEvent.Amount)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_balance_history: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleFundsWithdrawn(fundsWithdrawnEvent event.FundsWithdrawnEvent) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	_, err = tx.Exec(context.Background(), `
		UPDATE bank_balance 
		SET balance = $1, balance_timestamp = NOW() 
		WHERE account_id = $2`, fundsWithdrawnEvent.TotalBalance, fundsWithdrawnEvent.AccountID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to update bank_balance: %v", err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp)
		VALUES ($1, 'withdrawal', $2, NOW())`, fundsWithdrawnEvent.AccountID, -fundsWithdrawnEvent.Amount)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to insert into bank_balance_history: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleEmailContactUpserted(userContactInfoUpsertedEvent event.EmailContactUpserted) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	var userID int
	err = tx.QueryRow(context.Background(), `
		SELECT user_id 
		FROM bank_accounts 
		WHERE account_id = $1`, userContactInfoUpsertedEvent.AccountID).Scan(&userID)
	if err != nil {
		return fmt.Errorf("could not find user_id for account_id %v: %v", userContactInfoUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_email_contacts (user_id, email, created_at, updated_at)
		VALUES ($1, $2, NOW(), NOW()) 
		ON CONFLICT (user_id) 
		DO UPDATE SET email = $2, updated_at = NOW()`, userID, userContactInfoUpsertedEvent.Email)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert user contact info: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandlePhoneNumberContactUpserted(phoneNumberUpsertedEvent event.PhoneNumberContactUpserted) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	var userID int
	err = tx.QueryRow(context.Background(), `
		SELECT user_id 
		FROM bank_accounts 
		WHERE account_id = $1`, phoneNumberUpsertedEvent.AccountID).Scan(&userID)
	if err != nil {
		return fmt.Errorf("could not find user_id for account_id %v: %v", phoneNumberUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_phone_number_contacts (user_id, phone_number, type, updated_at)
		VALUES ($1, $2, $3, NOW()) 
		ON CONFLICT (user_id) 
		DO UPDATE SET 
			phone_number = EXCLUDED.phone_number, 
			type = EXCLUDED.type, 
			updated_at = NOW()`, userID, phoneNumberUpsertedEvent.PhoneNumber, phoneNumberUpsertedEvent.PhoneType)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert phone number: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleAddressUpserted(addressUpsertedEvent event.AddressUpserted) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	var userID int
	err = tx.QueryRow(context.Background(), `
		SELECT user_id 
		FROM bank_accounts 
		WHERE account_id = $1`, addressUpsertedEvent.AccountID).Scan(&userID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("could not find user_id for account_id %v: %v", addressUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_addresses 
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
		addressUpsertedEvent.UpdatedAt)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert address: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func (h *Handler) HandleEmergencyContactUpserted(emergencyContactUpsertedEvent event.EmergencyContactUpserted) error {
	db, err := db.NewClient()
	if err != nil {
		return fmt.Errorf("could not connect to database: %w", err)
	}

	tx, err := db.Begin(context.Background())
	if err != nil {
		return fmt.Errorf("could not begin transaction: %w", err)
	}

	var userID int
	err = tx.QueryRow(context.Background(), `
		SELECT user_id 
		FROM bank_accounts 
		WHERE account_id = $1`, emergencyContactUpsertedEvent.AccountID).Scan(&userID)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("could not find user_id for account_id %v: %v", emergencyContactUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_emergency_contacts (user_id, contact_name, contact_phone, created_at, updated_at)
		VALUES ($1, $2, $3, NOW(), NOW()) 
		ON CONFLICT (user_id) 
		DO UPDATE SET 
			contact_name = EXCLUDED.contact_name,
			contact_phone = EXCLUDED.contact_phone,
			updated_at = NOW()`,
		userID,
		emergencyContactUpsertedEvent.ContactName,
		emergencyContactUpsertedEvent.ContactPhone)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert emergency contact: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}
