package json

import (
	"encoding/json"
	"fmt"
)

func ConvertJsonMapToStruct(mapData map[string]interface{}, v interface{}) (interface{}, error) {
	eventJSON, err := json.Marshal(mapData)
	if err != nil {
		return nil, fmt.Errorf("error marshalling event data: %v", err)
	}

	err = json.Unmarshal(eventJSON, &v)
	if err != nil {
		return nil, fmt.Errorf("error unmarshalling event data: %v", err)
	}

	return &v, nil
}
