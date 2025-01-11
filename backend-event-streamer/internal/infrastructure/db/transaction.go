package db

import (
	"context"
	"fmt"

	"github.com/jackc/pgx/v4"
)

type DBTransaction interface {
	ExecuteQuery(dest any, query string, args ...any) error
	ExecuteQueryRowAsMap(query string, args ...any) (*ResultMap, error)
	ExecuteQueryRows(dest any, query string, args ...any) error
	ExecuteCommand(query string, args ...any) (int, error)
	Commit() error
	Rollback() error
}

type DBTransactionImpl struct {
	tx pgx.Tx
}

func NewDBTransaction(tx pgx.Tx) DBTransaction {
	return &DBTransactionImpl{tx: tx}
}

func (t *DBTransactionImpl) ExecuteQuery(dest any, query string, args ...any) error {
	row := t.tx.QueryRow(context.Background(), query, args...)
	if err := MapRowToStruct(row, dest); err != nil {
		return fmt.Errorf("failed to execute query: %w", err)
	}
	return nil
}

func (t *DBTransactionImpl) ExecuteQueryRowAsMap(query string, args ...any) (*ResultMap, error) {
	rows, err := t.tx.Query(context.Background(), query, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to execute query: %w", err)
	}
	defer rows.Close()

	return MapRowToMap(rows)
}

func (t *DBTransactionImpl) ExecuteQueryRows(dest any, query string, args ...any) error {
	rows, err := t.tx.Query(context.Background(), query, args...)
	if err != nil {
		return fmt.Errorf("failed to execute query: %w", err)
	}
	defer rows.Close()

	if err := MapRowsToSlice(rows, dest); err != nil {
		return fmt.Errorf("failed to map rows to slice: %w", err)
	}

	return nil
}

func (t *DBTransactionImpl) ExecuteCommand(query string, args ...any) (int, error) {
	result, err := t.tx.Exec(context.Background(), query, args...)
	if err != nil {
		return 0, fmt.Errorf("failed to execute command: %w", err)
	}

	rowsAffected := result.RowsAffected()
	return int(rowsAffected), nil
}

func (t *DBTransactionImpl) Commit() error {
	return t.tx.Commit(context.Background())
}

func (t *DBTransactionImpl) Rollback() error {
	return t.tx.Rollback(context.Background())
}
