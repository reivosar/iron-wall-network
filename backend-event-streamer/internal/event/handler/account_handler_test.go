package handler_test

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/event/handler"
	"backend-event-streamer/internal/infrastructure/db"
	"backend-event-streamer/mocks"

	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

func TestHandleAccountCreated(t *testing.T) {
	t.Run("should handle account created successfully", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountCreatedEvent := event.AccountCreatedEvent{
			AccountID: "account123",
			Username:  "testuser",
			FullName:  "Test User",
		}
		expectedUserID := 1

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteQuery",
			mock.Anything,
			`INSERT INTO bank_users (created_at, updated_at) VALUES (NOW(), NOW()) RETURNING user_id`,
		).Run(func(args mock.Arguments) {
			*(args[0].(*int)) = expectedUserID
		}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_user_profiles (user_id, username, full_name, created_at, updated_at) VALUES ($1, $2, $3, NOW(), NOW())`,
			expectedUserID, accountCreatedEvent.Username, accountCreatedEvent.FullName,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_accounts (account_id, user_id, created_at, updated_at) VALUES ($1, $2, NOW(), NOW())`,
			accountCreatedEvent.AccountID, expectedUserID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO pending_accounts (account_id, pended_at) VALUES ($1, NOW())`,
			accountCreatedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_account_status (account_id, status, created_at, updated_at) VALUES ($1, 'Pending', NOW(), NOW())`,
			accountCreatedEvent.AccountID,
		).Return(1, nil).Once()

		// WHEN
		err := eventHandler.HandleAccountCreated(accountCreatedEvent)

		// THEN
		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAccountApproved(t *testing.T) {
	t.Run("should handle account approved successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountApprovedEvent := event.AccountApprovedEvent{
			AccountID: "account123",
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO approved_accounts (account_id, approved_at) VALUES ($1, NOW())`,
			accountApprovedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_account_status SET status = 'Approved', updated_at = NOW() WHERE account_id = $1`,
			accountApprovedEvent.AccountID,
		).Return(1, nil).Once()

		err := eventHandler.HandleAccountApproved(accountApprovedEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAccountPended(t *testing.T) {
	t.Run("should handle account pended successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountPendedEvent := event.AccountPendedEvent{
			AccountID: "account123",
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO pending_accounts (account_id, pended_at) VALUES ($1, NOW())`,
			accountPendedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_account_status SET status = 'Pending', updated_at = NOW() WHERE account_id = $1`,
			accountPendedEvent.AccountID,
		).Return(1, nil).Once()

		err := eventHandler.HandleAccountPended(accountPendedEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAccountActivated(t *testing.T) {
	t.Run("should handle account activated successfully", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountActivatedEvent := event.AccountActivatedEvent{
			AccountID: "account123",
			Password:  "hashed_password",
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`DELETE FROM pending_accounts WHERE account_id = $1`,
			accountActivatedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO active_accounts (account_id, activated_at) VALUES ($1, NOW())`,
			accountActivatedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_account_status SET status = 'Active', updated_at = NOW() WHERE account_id = $1`,
			accountActivatedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_balance (account_id, balance, balance_timestamp) VALUES ($1, 0, NOW())`,
			accountActivatedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_account_credentials (account_id, password_hash, expires_at, created_at, updated_at)
			VALUES ($1, $2, NOW() + INTERVAL '1 month', NOW(), NOW())`,
			accountActivatedEvent.AccountID, accountActivatedEvent.Password,
		).Return(1, nil).Once()

		// WHEN
		err := eventHandler.HandleAccountActivated(accountActivatedEvent)

		// THEN
		assert.NoError(t, err, "Expected no error, but got one")
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAccountSuspended(t *testing.T) {
	t.Run("should handle account suspended successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountSuspendedEvent := event.AccountSuspendedEvent{
			AccountID: "account123",
			Reason:    "Violation of terms",
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO suspend_accounts (account_id, suspended_at, suspension_reason) VALUES ($1, NOW(), $2)`,
			accountSuspendedEvent.AccountID, accountSuspendedEvent.Reason,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_account_status SET status = 'Suspended', updated_at = NOW() WHERE account_id = $1`,
			accountSuspendedEvent.AccountID,
		).Return(1, nil).Once()

		err := eventHandler.HandleAccountSuspended(accountSuspendedEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAccountClosed(t *testing.T) {
	t.Run("should handle account closed successfully", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		accountClosedEvent := event.AccountClosedEvent{
			AccountID: "account123",
			Reason:    "User requested closure",
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO closed_accounts (account_id, closed_at, closure_reason) VALUES ($1, NOW(), $2)`,
			accountClosedEvent.AccountID, accountClosedEvent.Reason,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_account_status SET status = 'Closed', updated_at = NOW() WHERE account_id = $1`,
			accountClosedEvent.AccountID,
		).Return(1, nil).Once()

		// WHEN
		err := eventHandler.HandleAccountClosed(accountClosedEvent)

		// THEN
		assert.NoError(t, err, "Expected no error, but got one")
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleFundsDeposited(t *testing.T) {
	t.Run("should handle funds deposited successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		fundsDepositedEvent := event.FundsDepositedEvent{
			AccountID:    "account123",
			Amount:       500,
			TotalBalance: 1500,
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_balance SET balance = $1, balance_timestamp = NOW() WHERE account_id = $2`,
			fundsDepositedEvent.TotalBalance, fundsDepositedEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp) VALUES ($1,'deposit',$2, NOW())`,
			fundsDepositedEvent.AccountID, fundsDepositedEvent.Amount,
		).Return(1, nil).Once()

		err := eventHandler.HandleFundsDeposited(fundsDepositedEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleFundsWithdrawn(t *testing.T) {
	t.Run("should handle funds withdrawn successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		fundsWithdrawnEvent := event.FundsWithdrawnEvent{
			AccountID:    "account123",
			Amount:       200,
			TotalBalance: 1300,
		}

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteCommand",
			`UPDATE bank_balance SET balance = $1, balance_timestamp = NOW() WHERE account_id = $2`,
			fundsWithdrawnEvent.TotalBalance, fundsWithdrawnEvent.AccountID,
		).Return(1, nil).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_balance_history (account_id, transaction_type, balance, balance_timestamp) VALUES ($1, 'withdrawal', $2, NOW())`,
			fundsWithdrawnEvent.AccountID, -fundsWithdrawnEvent.Amount,
		).Return(1, nil).Once()

		err := eventHandler.HandleFundsWithdrawn(fundsWithdrawnEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleAddressUpserted(t *testing.T) {
	t.Run("should handle address upserted successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		buildingName := "Test Building"
		addressEvent := event.AddressUpserted{
			AccountID:    "account123",
			PostalCode:   "123-4567",
			Prefecture:   "Tokyo",
			City:         "Shibuya",
			TownArea:     "Dogenzaka",
			BuildingName: &buildingName,
			AddressType:  "Home",
			UpdatedAt:    "2025-01-01 00:00:00",
		}
		expectedUserID := 1

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteQueryRowAsMap",
			mock.MatchedBy(func(query string) bool {
				return query == `SELECT user_id FROM bank_accounts WHERE account_id = $1`
			}),
			"account123",
		).Return(
			map[string]any{
				"user_id": expectedUserID,
			},
			nil,
		).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_user_addresses 
				(user_id, postal_code, prefecture, city, town_area, building_name, address_type, created_at, updated_at)
			VALUES 
				($1, $2, $3, $4, $5, $6, $7, NOW(), $8) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				building_name = $6, 
				address_type = $7,
				updated_at = $8`,
			expectedUserID,
			addressEvent.PostalCode,
			addressEvent.Prefecture,
			addressEvent.City,
			addressEvent.TownArea,
			addressEvent.BuildingName,
			addressEvent.AddressType,
			addressEvent.UpdatedAt,
		).Return(1, nil).Once()

		err := eventHandler.HandleAddressUpserted(addressEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleEmergencyContactUpserted(t *testing.T) {
	t.Run("should handle emergency contact upserted successfully", func(t *testing.T) {
		// GIVEN
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		emergencyContactEvent := event.EmergencyContactUpserted{
			AccountID:    "account123",
			ContactName:  "Emergency Contact",
			ContactPhone: "123-456-7890",
		}

		expectedUserID := 1

		// Mock transaction behavior
		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		// Mock ExecuteQuery
		mockTx.On("ExecuteQueryRowAsMap",
			mock.MatchedBy(func(query string) bool {
				return query == `SELECT user_id FROM bank_accounts WHERE account_id = $1`
			}),
			"account123",
		).Return(
			map[string]any{
				"user_id": expectedUserID,
			},
			nil,
		).Once()

		// Mock ExecuteCommand for the upsert
		mockTx.On("ExecuteCommand", mock.MatchedBy(func(query string) bool {
			return query ==
				`INSERT INTO bank_user_emergency_contacts (user_id, contact_name, contact_phone, created_at, updated_at) VALUES ($1, $2, $3, NOW(), NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				contact_name = EXCLUDED.contact_name,
				contact_phone = EXCLUDED.contact_phone,
				updated_at = NOW()`
		}), expectedUserID, emergencyContactEvent.ContactName, emergencyContactEvent.ContactPhone).Return(1, nil).Once()

		// WHEN
		err := eventHandler.HandleEmergencyContactUpserted(emergencyContactEvent)

		// THEN
		assert.NoError(t, err, "Expected no error, but got one")
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandlePhoneNumberContactUpserted(t *testing.T) {
	t.Run("should handle phone number contact upserted successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		phoneNumberEvent := event.PhoneNumberContactUpserted{
			AccountID:   "account123",
			PhoneNumber: "123-456-7890",
			PhoneType:   "Mobile",
		}
		expectedUserID := 1

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteQueryRowAsMap",
			mock.MatchedBy(func(query string) bool {
				return query == `SELECT user_id FROM bank_accounts WHERE account_id = $1`
			}),
			"account123",
		).Return(
			map[string]any{
				"user_id": expectedUserID,
			},
			nil,
		).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_user_phone_number_contacts (user_id, phone_number, type, updated_at)
			VALUES ($1, $2, $3, NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET 
				phone_number = EXCLUDED.phone_number, 
				type = EXCLUDED.type, 
				updated_at = NOW()`,
			expectedUserID, phoneNumberEvent.PhoneNumber, phoneNumberEvent.PhoneType,
		).Return(1, nil).Once()

		err := eventHandler.HandlePhoneNumberContactUpserted(phoneNumberEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}

func TestHandleEmailContactUpserted(t *testing.T) {
	t.Run("should handle email contact upserted successfully", func(t *testing.T) {
		mockDBClient := new(mocks.MockDBClient)
		mockTx := new(mocks.MockDBTransaction)
		eventHandler := handler.NewAccountEventHandler(mockDBClient)

		emailContactEvent := event.EmailContactUpserted{
			AccountID: "account123",
			Email:     "test@example.com",
		}
		expectedUserID := 1

		mockDBClient.On("WithTransaction", mock.AnythingOfType("func(db.DBTransaction) error")).
			Run(func(args mock.Arguments) {
				txFunc := args.Get(0).(func(db.DBTransaction) error)
				_ = txFunc(mockTx)
			}).Return(nil).Once()

		mockTx.On("ExecuteQueryRowAsMap",
			mock.MatchedBy(func(query string) bool {
				return query == `SELECT user_id FROM bank_accounts WHERE account_id = $1`
			}),
			"account123",
		).Return(
			map[string]any{
				"user_id": expectedUserID,
			},
			nil,
		).Once()

		mockTx.On("ExecuteCommand",
			`INSERT INTO bank_user_email_contacts (user_id, email, created_at, updated_at) VALUES ($1, $2, NOW(), NOW()) 
			ON CONFLICT (user_id) 
			DO UPDATE SET email = $2, updated_at = NOW()`,
			expectedUserID, emailContactEvent.Email,
		).Return(1, nil).Once()

		err := eventHandler.HandleEmailContactUpserted(emailContactEvent)

		assert.NoError(t, err)
		mockDBClient.AssertExpectations(t)
		mockTx.AssertExpectations(t)
	})
}
