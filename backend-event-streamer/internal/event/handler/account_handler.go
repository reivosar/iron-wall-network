package handler

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/infrastructure/db"
	"context"
	"fmt"
)

func HandleAccountCreated(accountCreatedEvent event.AccountCreatedEvent) error {
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

func HandleAccountApproved(accountApprovedEvent event.AccountApprovedEvent) error {
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

func HandleAccountPended(accountPendedEvent event.AccountPendedEvent) error {
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

func HandleAccountSuspended(accountSuspendedEvent event.AccountSuspendedEvent) error {
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

func HandleAccountActivated(accountActivatedEvent event.AccountActivatedEvent) error {
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

func HandleAccountClosed(accountClosedEvent event.AccountClosedEvent) error {
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

func HandleFundsDeposited(fundsDepositedEvent event.FundsDepositedEvent) error {
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

func HandleFundsWithdrawn(fundsWithdrawnEvent event.FundsWithdrawnEvent) error {
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

func HandleUserContactInfoUpserted(userContactInfoUpsertedEvent event.UserContactInfoUpsertedEvent) error {
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
		INSERT INTO bank_user_contacts (user_id, email, created_at, updated_at)
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

func HandlePhoneNumberUpserted(phoneNumberUpsertedEvent event.PhoneNumberUpsertedEvent) error {
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
		INSERT INTO bank_user_phone_numbers (user_id, phone_number, type)
		VALUES ($1, $2, $3) 
		ON CONFLICT (user_id, phone_number) 
		DO UPDATE SET type = $3`, userID, phoneNumberUpsertedEvent.PhoneNumber, phoneNumberUpsertedEvent.PhoneType)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert phone number: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func HandleAddressUpserted(addressUpsertedEvent event.AddressUpsertedEvent) error {
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
		return fmt.Errorf("could not find user_id for account_id %v: %v", addressUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_addresses (user_id, address, type)
		VALUES ($1, $2, $3) 
		ON CONFLICT (user_id, address) 
		DO UPDATE SET type = $3`, userID, addressUpsertedEvent.Address, addressUpsertedEvent.AddressType)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert address: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}

func HandleEmergencyContactUpserted(emergencyContactUpsertedEvent event.EmergencyContactUpsertedEvent) error {
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
		return fmt.Errorf("could not find user_id for account_id %v: %v", emergencyContactUpsertedEvent.AccountID, err)
	}

	_, err = tx.Exec(context.Background(), `
		INSERT INTO bank_user_emergency_contacts (user_id, contact_name, contact_phone)
		VALUES ($1, $2, $3) 
		ON CONFLICT (user_id, contact_name) 
		DO UPDATE SET contact_phone = $3`, userID, emergencyContactUpsertedEvent.ContactName, emergencyContactUpsertedEvent.ContactPhone)
	if err != nil {
		tx.Rollback(context.Background())
		return fmt.Errorf("failed to upsert emergency contact: %v", err)
	}

	if err := tx.Commit(context.Background()); err != nil {
		return fmt.Errorf("failed to commit transaction: %v", err)
	}

	return nil
}
