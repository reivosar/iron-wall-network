package db

import (
	"database/sql"
	"fmt"
	"reflect"
	"sync"
)

var fieldCache sync.Map

func MapRowToStruct(row *sql.Row, dest any) error {
	val := reflect.ValueOf(dest)
	if val.Kind() != reflect.Ptr || val.Elem().Kind() != reflect.Struct {
		return fmt.Errorf("dest must be a pointer to a struct")
	}

	val = val.Elem()

	fields, err := getCachedFields(val.Type())
	if err != nil {
		return fmt.Errorf("failed to get fields for dest: %w", err)
	}

	scanArgs := make([]any, len(fields))
	for i, field := range fields {
		scanArgs[i] = val.FieldByIndex(field.Index).Addr().Interface()
	}

	if err := row.Scan(scanArgs...); err != nil {
		return fmt.Errorf("failed to scan row: %w", err)
	}

	return nil
}

func MapRowsToSlice(rows *sql.Rows, dest any) error {
	val := reflect.ValueOf(dest)
	if val.Kind() != reflect.Ptr || val.Elem().Kind() != reflect.Slice {
		return fmt.Errorf("dest must be a pointer to a slice")
	}

	val = val.Elem()
	elemType := val.Type().Elem()

	fields, err := getCachedFields(elemType)
	if err != nil {
		return fmt.Errorf("failed to get fields for dest: %w", err)
	}

	for rows.Next() {
		elemPtr := reflect.New(elemType).Elem()

		scanArgs := make([]any, len(fields))
		for i, field := range fields {
			scanArgs[i] = elemPtr.FieldByIndex(field.Index).Addr().Interface()
		}

		if err := rows.Scan(scanArgs...); err != nil {
			return fmt.Errorf("failed to scan row: %w", err)
		}

		val.Set(reflect.Append(val, elemPtr))
	}

	if err := rows.Err(); err != nil {
		return fmt.Errorf("error during rows iteration: %w", err)
	}

	return nil
}

func getCachedFields(structType reflect.Type) ([]reflect.StructField, error) {
	if cached, ok := fieldCache.Load(structType); ok {
		return cached.([]reflect.StructField), nil
	}

	numFields := structType.NumField()
	fields := make([]reflect.StructField, numFields)
	for i := 0; i < numFields; i++ {
		fields[i] = structType.Field(i)
	}

	fieldCache.Store(structType, fields)
	return fields, nil
}
