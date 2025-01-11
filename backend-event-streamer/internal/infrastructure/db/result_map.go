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

	switch v := value.(type) {
	case int:
		return v, nil
	case int32:
		return int(v), nil
	case int64:
		if v > int64(^uint(0)>>1) || v < -int64(^uint(0)>>1)-1 {
			return 0, fmt.Errorf("key %s value out of int range", key)
		}
		return int(v), nil
	default:
		return 0, fmt.Errorf("key %s is not of type int, int32, or int64", key)
	}
}

func (r *ResultMap) GetInt32(key string) (int32, error) {
	value, ok := r.data[key]
	if !ok {
		return 0, fmt.Errorf("key %s not found in result map", key)
	}

	intValue, ok := value.(int32)
	if !ok {
		return 0, fmt.Errorf("key %s is not of type int64", key)
	}

	return intValue, nil
}

func (r *ResultMap) GetInt64(key string) (int64, error) {
	value, ok := r.data[key]
	if !ok {
		return 0, fmt.Errorf("key %s not found in result map", key)
	}

	intValue, ok := value.(int64)
	if !ok {
		return 0, fmt.Errorf("key %s is not of type int64", key)
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
