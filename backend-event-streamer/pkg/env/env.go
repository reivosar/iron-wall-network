package env

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

// GetEnv retrieves an environment variable or returns a default value if the variable is not set.
func GetEnv(key string, defaultValue string) string {
	value, exists := os.LookupEnv(key)
	if !exists || value == "" {
		return defaultValue
	}
	return value
}

// GetEnvBool retrieves a boolean environment variable. If the variable is not set, it returns the default value.
func GetEnvBool(key string, defaultValue bool) bool {
	value := GetEnv(key, "")
	if value == "" {
		return defaultValue
	}
	return strings.ToLower(value) == "true"
}

// GetEnvInt retrieves an integer environment variable. If the variable is not set, it returns the default value.
func GetEnvInt(key string, defaultValue int) int {
	value := GetEnv(key, fmt.Sprintf("%d", defaultValue))
	intValue, err := strconv.Atoi(value)
	if err != nil {
		return defaultValue
	}
	return intValue
}

// GetEnvFloat retrieves a float environment variable. If the variable is not set, it returns the default value.
func GetEnvFloat(key string, defaultValue float64) float64 {
	value := GetEnv(key, fmt.Sprintf("%f", defaultValue))
	floatValue, err := strconv.ParseFloat(value, 64)
	if err != nil {
		return defaultValue
	}
	return floatValue
}
