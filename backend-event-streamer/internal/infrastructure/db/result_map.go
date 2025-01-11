package db

import "fmt"

type ResultMap struct {
	data map[string]any
}

func NewResultMap(data map[string]any) *ResultMap {
	return &ResultMap{data: data}
}

func (r *ResultMap) GetInt(key string) (int, error) {
	value, ok := r.data[key]
	if !ok {
		return 0, fmt.Errorf("key %s not found in result map", key)
	}

	intValue, ok := value.(int)
	if !ok {
		return 0, fmt.Errorf("key %s is not of type int", key)
	}

	return intValue, nil
}

func (r *ResultMap) GetString(key string) (string, error) {
	value, ok := r.data[key]
	if !ok {
		return "", fmt.Errorf("key %s not found in result map", key)
	}

	strValue, ok := value.(string)
	if !ok {
		return "", fmt.Errorf("key %s is not of type string", key)
	}

	return strValue, nil
}

func (r *ResultMap) HasKey(key string) bool {
	_, ok := r.data[key]
	return ok
}

func (r *ResultMap) IsEmpty() bool {
	return len(r.data) == 0
}

func (r *ResultMap) IsNotEmpty() bool {
	return len(r.data) != 0
}
