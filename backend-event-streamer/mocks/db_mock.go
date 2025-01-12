package mocks

import (
	"backend-event-streamer/internal/infrastructure/db"

	"github.com/stretchr/testify/mock"
)

type MockDBClient struct {
	mock.Mock
}

type MockDBTransaction struct {
	mock.Mock
}

func (m *MockDBClient) FetchOne(dest any, query string, args ...any) error {
	argsMock := m.Called(append([]any{dest, query}, args...)...)
	return argsMock.Error(0)
}

func (m *MockDBClient) FetchAll(dest any, query string, args ...any) error {
	argsMock := m.Called(append([]any{dest, query}, args...)...)
	return argsMock.Error(0)
}

func (m *MockDBClient) FetchOneAsMap(query string, args ...any) (*db.ResultMap, error) {
	argsMock := m.Called(append([]any{query}, args...)...)
	result := argsMock.Get(0)
	if result == nil {
		return nil, argsMock.Error(1)
	}
	return db.NewResultMap(result.(map[string]any)), argsMock.Error(1)
}

func (m *MockDBClient) WithTransaction(action func(tx db.DBTransaction) error) error {
	argsMock := m.Called(action)
	return argsMock.Error(0)
}

func (m *MockDBTransaction) ExecuteQuery(dest any, query string, args ...any) error {
	argsMock := m.Called(append([]any{dest, query}, args...)...)
	return argsMock.Error(0)
}

func (m *MockDBTransaction) ExecuteQueryRows(dest any, query string, args ...any) error {
	argsMock := m.Called(append([]any{dest, query}, args...)...)
	return argsMock.Error(0)
}

func (m *MockDBTransaction) ExecuteQueryRowAsMap(query string, args ...any) (*db.ResultMap, error) {
	argsMock := m.Called(append([]any{query}, args...)...)
	result := argsMock.Get(0)
	if result == nil {
		return nil, argsMock.Error(1)
	}
	return db.NewResultMap(result.(map[string]any)), argsMock.Error(1)
}

func (m *MockDBTransaction) ExecuteCommand(query string, args ...any) (int, error) {
	argsMock := m.Called(append([]any{query}, args...)...)
	return argsMock.Int(0), argsMock.Error(1)
}

func (m *MockDBTransaction) Commit() error {
	return m.Called().Error(0)
}

func (m *MockDBTransaction) Rollback() error {
	return m.Called().Error(0)
}
