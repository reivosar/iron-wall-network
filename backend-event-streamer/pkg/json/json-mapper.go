package jsonmapper

import (
	"encoding/json"
	"fmt"
)

func JsonMapToStruct(mapData map[string]interface{}, v interface{}) error {
	eventJSON, err := json.Marshal(mapData)
	if err != nil {
		return fmt.Errorf("error marshalling event data: %v", err)
	}

	err = json.Unmarshal(eventJSON, &v)
	if err != nil {
		return fmt.Errorf("error unmarshalling event data: %v", err)
	}

	return nil
}
