package db

import (
	"database/sql"
	"fmt"
)

type DBTransaction interface {
	ExecuteQuery(dest any, query string, args ...any) error
	ExecuteQueryRows(dest any, query string, args ...any) error
	ExecuteCommand(query string, args ...any) (int, error)
	Commit() error
	Rollback() error
}

type DBTransactionImpl struct {
	tx *sql.Tx
}

func NewDBTransaction(tx *sql.Tx) DBTransaction {
	return &DBTransactionImpl{tx: tx}
}

func (t *DBTransactionImpl) ExecuteQuery(dest any, query string, args ...any) error {
	row := t.tx.QueryRow(query, args...)
	if err := MapRowToStruct(row, dest); err != nil {
		return fmt.Errorf("failed to execute query: %w", err)
	}
	return nil
}

func (t *DBTransactionImpl) ExecuteQueryRows(dest any, query string, args ...any) error {
	rows, err := t.tx.Query(query, args...)
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
	result, err := t.tx.Exec(query, args...)
	if err != nil {
		return 0, fmt.Errorf("failed to execute command: %w", err)
	}

	rowsAffected, err := result.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("failed to fetch rows affected: %w", err)
	}

	return int(rowsAffected), nil
}

func (t *DBTransactionImpl) Commit() error {
	return t.tx.Commit()
}

func (t *DBTransactionImpl) Rollback() error {
	return t.tx.Rollback()
}
