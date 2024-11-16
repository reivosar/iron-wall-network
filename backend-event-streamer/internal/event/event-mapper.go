package event

import (
	"encoding/json"
	"fmt"
)

func MapToEvent(eventData map[string]interface{}, v interface{}) error {
	eventJSON, err := json.Marshal(eventData)
	if err != nil {
		return fmt.Errorf("error marshalling event data: %v", err)
	}

	err = json.Unmarshal(eventJSON, &v)
	if err != nil {
		return fmt.Errorf("error unmarshalling event data: %v", err)
	}

	return nil
}
