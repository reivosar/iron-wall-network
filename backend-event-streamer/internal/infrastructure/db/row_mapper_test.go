package db

import (
	"fmt"
	"testing"

	"github.com/jackc/pgconn"
	"github.com/jackc/pgproto3/v2"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/mock"
)

type MockRow struct {
	mock.Mock
}

func (m *MockRow) Scan(dest ...any) error {
	argsMock := m.Called(dest...)
	return argsMock.Error(0)
}

type MockRows struct {
	mock.Mock
}

func (m *MockRows) Next() bool {
	args := m.Called()
	return args.Bool(0)
}

func (m *MockRows) FieldDescriptions() []pgproto3.FieldDescription {
	args := m.Called()
	return args.Get(0).([]pgproto3.FieldDescription)
}

func (m *MockRows) Scan(dest ...any) error {
	args := m.Called(dest)
	return args.Error(0)
}

func (m *MockRows) Err() error {
	args := m.Called()
	return args.Error(0)
}

func (m *MockRows) Close() {
	m.Called()
}

func (m *MockRows) CommandTag() pgconn.CommandTag {
	args := m.Called()
	return args.Get(0).(pgconn.CommandTag)
}

func (m *MockRows) Values() ([]any, error) {
	args := m.Called()
	return args.Get(0).([]any), args.Error(1)
}

func (m *MockRows) RawValues() [][]byte {
	args := m.Called()
	return args.Get(0).([][]byte)
}

func TestMapRowToStruct(t *testing.T) {
	t.Run("should map row to struct successfully", func(t *testing.T) {
		// GIVEN
		mockRow := new(MockRow)
		mockRow.On("Scan", mock.Anything).Run(func(args mock.Arguments) {
			*(args[0].(*int)) = 42
		}).Return(nil)

		type TestStruct struct {
			ID int `db:"id"`
		}
		var result TestStruct

		// WHEN
		err := MapRowToStruct(mockRow, &result)

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, 42, result.ID)
	})

	t.Run("should return error if dest is not a pointer", func(t *testing.T) {
		// GIVEN
		mockRow := new(MockRow)
		mockRow.On("Scan", mock.Anything).Return(nil)

		type TestStruct struct {
			ID int `db:"id"`
		}
		result := TestStruct{}

		// WHEN
		err := MapRowToStruct(mockRow, result)

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "dest must be a pointer")
	})

	t.Run("should map row to scalar value", func(t *testing.T) {
		// GIVEN
		mockRow := new(MockRow)
		mockRow.On("Scan", mock.Anything).Run(func(args mock.Arguments) {
			*(args[0].(*int)) = 42
		}).Return(nil)

		var result int

		// WHEN
		err := MapRowToStruct(mockRow, &result)

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, 42, result)
	})

	t.Run("should return error if row.Scan fails", func(t *testing.T) {
		// GIVEN
		mockRow := new(MockRow)
		mockRow.On("Scan", mock.Anything).Return(fmt.Errorf("scan error"))

		type TestStruct struct {
			ID int `db:"id"`
		}
		var result TestStruct

		// WHEN
		err := MapRowToStruct(mockRow, &result)

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "failed to scan row")
	})

	t.Run("should handle structs with no exported fields without error", func(t *testing.T) {
		// GIVEN
		mockRow := new(MockRow)
		mockRow.On("Scan", mock.Anything).Return(nil)

		type NoPublicElementsStruct struct {
			_ func()
		}
		var result NoPublicElementsStruct

		// WHEN
		err := MapRowToStruct(mockRow, &result)

		// THEN
		assert.NoError(t, err)
	})
}

func TestMapRowsToSlice(t *testing.T) {
	t.Run("should map rows to slice of struct successfully", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(true).Once()
		mockRows.On("Next").Return(false).Once()
		mockRows.On("Scan", mock.Anything).Run(func(args mock.Arguments) {
			dest := args.Get(0).([]any)
			*dest[0].(*int) = 1
			*dest[1].(*string) = "John Doe"
		}).Return(nil)
		mockRows.On("Err").Return(nil)

		type TestStruct struct {
			ID   int    `db:"id"`
			Name string `db:"name"`
		}
		var result []TestStruct

		// WHEN
		err := MapRowsToSlice(mockRows, &result)

		// THEN
		assert.NoError(t, err)
		assert.Len(t, result, 1)
		assert.Equal(t, 1, result[0].ID)
		assert.Equal(t, "John Doe", result[0].Name)
	})

	t.Run("should return error if dest is not a pointer to a slice", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)

		type TestStruct struct {
			ID int `db:"id"`
		}
		var result TestStruct

		// WHEN
		err := MapRowsToSlice(mockRows, result)

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "dest must be a pointer to a slice")
	})

	t.Run("should return error if row scan fails", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(true).Once()
		mockRows.On("Scan", mock.Anything).Return(fmt.Errorf("scan error"))
		mockRows.On("Err").Return(nil)

		type TestStruct struct {
			ID int `db:"id"`
		}
		var result []TestStruct

		// WHEN
		err := MapRowsToSlice(mockRows, &result)

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "failed to scan row")
	})

	t.Run("should return error if rows.Err is not nil", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(false)
		mockRows.On("Err").Return(fmt.Errorf("rows error"))

		type TestStruct struct {
			ID int `db:"id"`
		}
		var result []TestStruct

		// WHEN
		err := MapRowsToSlice(mockRows, &result)

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "error during rows iteration")
	})
}

func TestMapRowToMap(t *testing.T) {
	t.Run("should map row to map successfully", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(true)
		mockRows.On("FieldDescriptions").Return([]pgproto3.FieldDescription{
			{Name: []byte("id")},
			{Name: []byte("name")},
		})
		mockRows.On("Scan", mock.Anything).Run(func(args mock.Arguments) {
			dest := args.Get(0).([]any)
			*dest[0].(*any) = 42
			*dest[1].(*any) = "John Doe"
		}).Return(nil)
		mockRows.On("Err").Return(nil)

		// WHEN
		result, err := MapRowToMap(mockRows)

		// THEN
		assert.NoError(t, err)
		assert.NotNil(t, result)
		id, err := result.GetInt("id")
		assert.NoError(t, err)
		assert.Equal(t, 42, id)

		name, err := result.GetString("name")
		assert.NoError(t, err)
		assert.Equal(t, "John Doe", name)
	})

	t.Run("should return error when no rows are found", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(false)
		mockRows.On("Err").Return(nil)

		// WHEN
		result, err := MapRowToMap(mockRows)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, result)
		assert.Contains(t, err.Error(), "no rows found")
	})

	t.Run("should return error when row scan fails", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(true)
		mockRows.On("FieldDescriptions").Return([]pgproto3.FieldDescription{
			{Name: []byte("id")},
			{Name: []byte("name")},
		})
		mockRows.On("Scan", mock.AnythingOfType("[]interface {}")).Return(fmt.Errorf("scan error"))

		// WHEN
		result, err := MapRowToMap(mockRows)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, result)
		assert.Contains(t, err.Error(), "failed to scan row")
	})

	t.Run("should return error when rows.Err is not nil", func(t *testing.T) {
		// GIVEN
		mockRows := new(MockRows)
		mockRows.On("Next").Return(false)
		mockRows.On("Err").Return(fmt.Errorf("rows error"))

		// WHEN
		result, err := MapRowToMap(mockRows)

		// THEN
		assert.Error(t, err)
		assert.Nil(t, result)
		assert.Contains(t, err.Error(), "failed to fetch row")
	})
}
