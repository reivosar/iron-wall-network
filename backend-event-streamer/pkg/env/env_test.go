package env

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetEnv(t *testing.T) {
	t.Run("should return the default value when the environment variable is not set", func(t *testing.T) {
		defaultValue := "default"
		key := "UNSET_ENV"
		assert.Equal(t, defaultValue, GetEnv(key, defaultValue))
	})

	t.Run("should return the environment variable value when it is set", func(t *testing.T) {
		key := "SET_ENV"
		expectedValue := "value"
		os.Setenv(key, expectedValue)
		defer os.Unsetenv(key)

		assert.Equal(t, expectedValue, GetEnv(key, "default"))
	})
}

func TestGetEnvBool(t *testing.T) {
	t.Run("should return the default value when the boolean environment variable is not set", func(t *testing.T) {
		key := "UNSET_BOOL_ENV"
		defaultValue := true
		assert.Equal(t, defaultValue, GetEnvBool(key, defaultValue))
	})

	t.Run("should return true when the environment variable value is 'true'", func(t *testing.T) {
		key := "BOOL_ENV"
		os.Setenv(key, "true")
		defer os.Unsetenv(key)

		assert.True(t, GetEnvBool(key, false))
	})

	t.Run("should return false when the environment variable value is not 'true'", func(t *testing.T) {
		key := "BOOL_ENV"
		os.Setenv(key, "false")
		defer os.Unsetenv(key)

		assert.False(t, GetEnvBool(key, true))
	})
}

func TestGetEnvInt(t *testing.T) {
	t.Run("should return the default value when the integer environment variable is not set", func(t *testing.T) {
		key := "UNSET_INT_ENV"
		defaultValue := 42
		assert.Equal(t, defaultValue, GetEnvInt(key, defaultValue))
	})

	t.Run("should return the integer value of the environment variable when it is set", func(t *testing.T) {
		key := "INT_ENV"
		os.Setenv(key, "100")
		defer os.Unsetenv(key)

		assert.Equal(t, 100, GetEnvInt(key, 42))
	})

	t.Run("should return the default value when the environment variable value is invalid for an integer", func(t *testing.T) {
		key := "INVALID_INT_ENV"
		os.Setenv(key, "invalid")
		defer os.Unsetenv(key)

		assert.Equal(t, 42, GetEnvInt(key, 42))
	})
}

func TestGetEnvFloat(t *testing.T) {
	t.Run("should return the default value when the float environment variable is not set", func(t *testing.T) {
		key := "UNSET_FLOAT_ENV"
		defaultValue := 3.14
		assert.Equal(t, defaultValue, GetEnvFloat(key, defaultValue))
	})

	t.Run("should return the float value of the environment variable when it is set", func(t *testing.T) {
		key := "FLOAT_ENV"
		os.Setenv(key, "2.71")
		defer os.Unsetenv(key)

		assert.Equal(t, 2.71, GetEnvFloat(key, 3.14))
	})

	t.Run("should return the default value when the environment variable value is invalid for a float", func(t *testing.T) {
		key := "INVALID_FLOAT_ENV"
		os.Setenv(key, "invalid")
		defer os.Unsetenv(key)

		assert.Equal(t, 3.14, GetEnvFloat(key, 3.14))
	})
}
