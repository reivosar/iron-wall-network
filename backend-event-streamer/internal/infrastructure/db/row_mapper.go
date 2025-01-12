package db

import (
	"fmt"
	"reflect"
	"sync"

	"github.com/jackc/pgx/v4"
)

var fieldCache sync.Map

func MapRowToStruct(row pgx.Row, dest any) error {
	val := reflect.ValueOf(dest)
	if val.Kind() != reflect.Ptr {
		return fmt.Errorf("dest must be a pointer")
	}

	if val.Elem().Kind() != reflect.Struct {
		if err := row.Scan(dest); err != nil {
			return fmt.Errorf("failed to scan row into scalar value: %w", err)
		}
		return nil
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

func MapRowsToSlice(rows pgx.Rows, dest any) error {
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

	var fields []reflect.StructField
	for i := 0; i < structType.NumField(); i++ {
		field := structType.Field(i)
		if field.PkgPath == "" {
			fields = append(fields, field)
		}
	}

	fieldCache.Store(structType, fields)
	return fields, nil
}

func MapRowToMap(rows pgx.Rows) (*ResultMap, error) {
	if !rows.Next() {
		if rows.Err() != nil {
			return nil, fmt.Errorf("failed to fetch row: %w", rows.Err())
		}
		return nil, fmt.Errorf("no rows found")
	}

	fieldDescriptions := rows.FieldDescriptions()
	columnNames := make([]string, len(fieldDescriptions))
	for i, fd := range fieldDescriptions {
		columnNames[i] = string(fd.Name)
	}

	values := make([]any, len(fieldDescriptions))
	valuePtrs := make([]any, len(fieldDescriptions))
	for i := range values {
		valuePtrs[i] = &values[i]
	}

	if err := rows.Scan(valuePtrs...); err != nil {
		return nil, fmt.Errorf("failed to scan row: %w", err)
	}

	result := make(map[string]any, len(columnNames))
	for i, colName := range columnNames {
		result[colName] = values[i]
	}

	return NewResultMap(result), nil
}
