package db

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetInt(t *testing.T) {
	t.Run("should return int value for a valid int key", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": 42})

		// WHEN
		value, err := resultMap.GetInt("key")

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, 42, value)
	})

	t.Run("should return int value for a valid int32 key", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int32(42)})

		// WHEN
		value, err := resultMap.GetInt("key")

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, 42, value)
	})

	t.Run("should return error if value is not of type 64", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int64(42)})

		// WHEN
		value, err := resultMap.GetInt("key")

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "is not of type int, int32")
		assert.Equal(t, 0, value)
	})

	t.Run("should return error if key does not exist", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": 42})

		// WHEN
		value, err := resultMap.GetInt("missingKey")

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "not found in result map")
		assert.Equal(t, 0, value)
	})

	t.Run("should return error if value is not of type int, int32", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "not an int"})

		// WHEN
		value, err := resultMap.GetInt("key")

		// THEN
		assert.Error(t, err)
		assert.Contains(t, err.Error(), "is not of type int, int32")
		assert.Equal(t, 0, value)
	})
}

func TestGetInt32(t *testing.T) {
	t.Run("should return int32 value for a valid int32 key", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int32(42)})

		// WHEN
		value, err := resultMap.GetInt32("key")

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, int32(42), value)
	})

	t.Run("should return error if key does not exist", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int32(42)})

		// WHEN
		value, err := resultMap.GetInt32("missingKey")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, int32(0), value)
	})

	t.Run("should return error if value is not of type int32", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "not an int32"})

		// WHEN
		value, err := resultMap.GetInt32("key")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, int32(0), value)
	})
}

func TestGetInt64(t *testing.T) {
	t.Run("should return int64 value for a valid int64 key", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int64(42)})

		// WHEN
		value, err := resultMap.GetInt64("key")

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, int64(42), value)
	})

	t.Run("should return error if key does not exist", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": int64(42)})

		// WHEN
		value, err := resultMap.GetInt64("missingKey")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, int64(0), value)
	})

	t.Run("should return error if value is not of type int64", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "not an int64"})

		// WHEN
		value, err := resultMap.GetInt64("key")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, int64(0), value)
	})
}

func TestGetString(t *testing.T) {
	t.Run("should return string value for a valid string key", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "value"})

		// WHEN
		value, err := resultMap.GetString("key")

		// THEN
		assert.NoError(t, err)
		assert.Equal(t, "value", value)
	})

	t.Run("should return error if key does not exist", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "value"})

		// WHEN
		value, err := resultMap.GetString("missingKey")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, "", value)
	})

	t.Run("should return error if value is not of type string", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": 42})

		// WHEN
		value, err := resultMap.GetString("key")

		// THEN
		assert.Error(t, err)
		assert.Equal(t, "", value)
	})
}

func TestHasKey(t *testing.T) {
	t.Run("should return true if key exists", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "value"})

		// WHEN
		result := resultMap.HasKey("key")

		// THEN
		assert.True(t, result)
	})

	t.Run("should return false if key does not exist", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "value"})

		// WHEN
		result := resultMap.HasKey("missingKey")

		// THEN
		assert.False(t, result)
	})
}

func TestIsEmpty(t *testing.T) {
	t.Run("should return true if result map is empty", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{})

		// WHEN
		result := resultMap.IsEmpty()

		// THEN
		assert.True(t, result)
	})

	t.Run("should return false if result map is not empty", func(t *testing.T) {
		// GIVEN
		resultMap := NewResultMap(map[string]any{"key": "value"})

		// WHEN
		result := resultMap.IsEmpty()

		// THEN
		assert.False(t, result)
	})
}
