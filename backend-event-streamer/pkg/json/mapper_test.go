package json

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

type TestStruct struct {
	Name  string `json:"name"`
	Age   int    `json:"age"`
	Email string `json:"email"`
}

func TestConvertJsonMapToStruct(t *testing.T) {
	tests := []struct {
		name      string
		inputMap  map[string]interface{}
		expected  TestStruct
		expectErr bool
	}{
		{
			name: "Valid input map",
			inputMap: map[string]interface{}{
				"name":  "John Doe",
				"age":   30,
				"email": "john.doe@example.com",
			},
			expected: TestStruct{
				Name:  "John Doe",
				Age:   30,
				Email: "john.doe@example.com",
			},
			expectErr: false,
		},
		{
			name: "Missing field in input map",
			inputMap: map[string]interface{}{
				"name": "Jane Doe",
				"age":  25,
			},
			expected: TestStruct{
				Name:  "Jane Doe",
				Age:   25,
				Email: "",
			},
			expectErr: false,
		},
		{
			name: "Invalid type in input map",
			inputMap: map[string]interface{}{
				"name":  "Invalid User",
				"age":   "not a number",
				"email": "invalid@example.com",
			},
			expected:  TestStruct{},
			expectErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			var result TestStruct
			converted, err := ConvertJsonMapToStruct(tc.inputMap, &result)

			if tc.expectErr {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tc.expected, result)
				assert.IsType(t, &TestStruct{}, converted)
			}
		})
	}
}
