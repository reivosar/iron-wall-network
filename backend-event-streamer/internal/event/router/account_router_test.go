package router_test

import (
	"backend-event-streamer/internal/event"
	"backend-event-streamer/internal/event/router"
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type MockAccountEventHandler struct {
	mock.Mock
}

func (m *MockAccountEventHandler) HandleAccountCreated(event event.AccountCreatedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAccountApproved(event event.AccountApprovedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAccountClosed(event event.AccountClosedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAccountActivated(event event.AccountActivatedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAccountPended(event event.AccountPendedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAccountSuspended(event event.AccountSuspendedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleAddressUpserted(event event.AddressUpserted) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleEmergencyContactUpserted(event event.EmergencyContactUpserted) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleFundsDeposited(event event.FundsDepositedEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleFundsWithdrawn(event event.FundsWithdrawnEvent) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandlePhoneNumberContactUpserted(event event.PhoneNumberContactUpserted) error {
	args := m.Called(event)
	return args.Error(0)
}

func (m *MockAccountEventHandler) HandleEmailContactUpserted(event event.EmailContactUpserted) error {
	args := m.Called(event)
	return args.Error(0)
}

func TestRouteAccountEvent(t *testing.T) {
	mockHandler := &MockAccountEventHandler{}
	r := router.NewAccountEventRouter(mockHandler)

	tests := []struct {
		name      string
		eventType string
		eventData map[string]interface{}
		expected  error
		mockCall  func()
	}{
		{
			name:      "AccountCreated",
			eventType: "AccountCreated",
			eventData: map[string]interface{}{
				"email":     "test@example.com",
				"fullName":  "Test User",
				"username":  "testuser",
				"accountId": "12345",
				"createdAt": "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountCreated", mock.AnythingOfType("event.AccountCreatedEvent")).Return(nil)
			},
		},
		{
			name:      "AccountApproved",
			eventType: "AccountApproved",
			eventData: map[string]interface{}{
				"accountId":     "12345",
				"approvedAt":    "2025-01-10T00:00:00Z",
				"approvalNotes": "All good.",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountApproved", mock.AnythingOfType("event.AccountApprovedEvent")).Return(nil)
			},
		},
		{
			name:      "AccountPended",
			eventType: "AccountPended",
			eventData: map[string]interface{}{
				"accountId": "12345",
				"reason":    "Verification pending.",
				"pendedAt":  "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountPended", mock.AnythingOfType("event.AccountPendedEvent")).Return(nil)
			},
		},
		{
			name:      "AccountSuspended",
			eventType: "AccountSuspended",
			eventData: map[string]interface{}{
				"accountId":   "12345",
				"reason":      "Fraud detected.",
				"suspendedAt": "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountSuspended", mock.AnythingOfType("event.AccountSuspendedEvent")).Return(nil)
			},
		},
		{
			name:      "AccountActivated",
			eventType: "AccountActivated",
			eventData: map[string]interface{}{
				"accountId":   "12345",
				"password":    "securepassword",
				"activatedAt": "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountActivated", mock.AnythingOfType("event.AccountActivatedEvent")).Return(nil)
			},
		},
		{
			name:      "AccountClosed",
			eventType: "AccountClosed",
			eventData: map[string]interface{}{
				"accountId": "12345",
				"reason":    "Customer request.",
				"closedAt":  "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAccountClosed", mock.AnythingOfType("event.AccountClosedEvent")).Return(nil)
			},
		},
		{
			name:      "FundsDeposited",
			eventType: "FundsDeposited",
			eventData: map[string]interface{}{
				"accountId":    "12345",
				"amount":       100.0,
				"totalBalance": 1000.0,
				"depositedAt":  "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleFundsDeposited", mock.AnythingOfType("event.FundsDepositedEvent")).Return(nil)
			},
		},
		{
			name:      "FundsWithdrawn",
			eventType: "FundsWithdrawn",
			eventData: map[string]interface{}{
				"accountId":    "12345",
				"amount":       50.0,
				"totalBalance": 950.0,
				"withdrawnAt":  "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleFundsWithdrawn", mock.AnythingOfType("event.FundsWithdrawnEvent")).Return(nil)
			},
		},
		{
			name:      "EmailContactUpserted",
			eventType: "EmailContactUpserted",
			eventData: map[string]interface{}{
				"accountId": "12345",
				"email":     "newemail@example.com",
				"updatedAt": "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleEmailContactUpserted", mock.AnythingOfType("event.EmailContactUpserted")).Return(nil)
			},
		},
		{
			name:      "PhoneNumberContactUpserted",
			eventType: "PhoneNumberContactUpserted",
			eventData: map[string]interface{}{
				"accountId":   "12345",
				"phoneNumber": "123-456-7890",
				"phoneType":   "mobile",
				"updatedAt":   "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandlePhoneNumberContactUpserted", mock.AnythingOfType("event.PhoneNumberContactUpserted")).Return(nil)
			},
		},
		{
			name:      "AddressUpserted",
			eventType: "AddressUpserted",
			eventData: map[string]interface{}{
				"accountId":    "12345",
				"postalCode":   "100-0001",
				"prefecture":   "Tokyo",
				"city":         "Chiyoda",
				"townArea":     "Marunouchi",
				"buildingName": "Building A",
				"addressType":  "home",
				"updatedAt":    "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleAddressUpserted", mock.AnythingOfType("event.AddressUpserted")).Return(nil)
			},
		},
		{
			name:      "EmergencyContactUpserted",
			eventType: "EmergencyContactUpserted",
			eventData: map[string]interface{}{
				"accountId":    "12345",
				"contactName":  "John Doe",
				"contactPhone": "987-654-3210",
				"updatedAt":    "2025-01-10T00:00:00Z",
			},
			expected: nil,
			mockCall: func() {
				mockHandler.On("HandleEmergencyContactUpserted", mock.AnythingOfType("event.EmergencyContactUpserted")).Return(nil)
			},
		},
		{
			name:      "UnknownEventType",
			eventType: "UnknownEvent",
			eventData: map[string]interface{}{},
			expected:  errors.New("unknown event type: UnknownEvent"),
			mockCall:  func() {},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tt.mockCall()

			err := r.RouteAccountEvent(tt.eventType, tt.eventData)

			if tt.expected != nil {
				assert.EqualError(t, err, tt.expected.Error())
			} else {
				assert.NoError(t, err)
			}

			mockHandler.AssertExpectations(t)
		})
	}
}
