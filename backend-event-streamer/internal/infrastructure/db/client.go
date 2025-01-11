package db

import (
	"backend-event-streamer/pkg/env"
	"database/sql"
	"fmt"
)

type DBClient interface {
	FetchOne(dest any, query string, args ...any) error
	FetchAll(dest any, query string, args ...any) error
	WithTransaction(action func(tx DBTransaction) error) error
}

type PostgresClient struct{}

func NewDBClient() DBClient {
	return &PostgresClient{}
}

func (p *PostgresClient) connect() (*sql.DB, error) {
	dbUser := env.GetEnv("BACKEND_DB_USER", "iron_wall_network_user")
	dbPassword := env.GetEnv("BACKEND_DB_PASSWORD", "password")
	dbHost := env.GetEnv("BACKEND_DB_HOST", "localhost")
	dbName := env.GetEnv("BACKEND_DB_NAME", "iron_wall_network_db")

	connStr := fmt.Sprintf("postgresql://%s:%s@%s:5432/%s?sslmode=disable", dbUser, dbPassword, dbHost, dbName)

	db, err := sql.Open("postgres", connStr)
	if err != nil {
		return nil, fmt.Errorf("unable to connect to database: %w", err)
	}

	if err := db.Ping(); err != nil {
		db.Close()
		return nil, fmt.Errorf("unable to ping database: %w", err)
	}

	return db, nil
}

func (p *PostgresClient) FetchOne(dest any, query string, args ...any) error {
	db, err := p.connect()
	if err != nil {
		return fmt.Errorf("failed to connect to the database: %w", err)
	}
	defer db.Close()

	row := db.QueryRow(query, args...)

	if err := MapRowToStruct(row, dest); err != nil {
		return fmt.Errorf("failed to map row to struct: %w", err)
	}

	return nil
}

func (p *PostgresClient) FetchAll(dest any, query string, args ...any) error {
	db, err := p.connect()
	if err != nil {
		return fmt.Errorf("failed to connect to the database: %w", err)
	}
	defer db.Close()

	rows, err := db.Query(query, args...)
	if err != nil {
		return fmt.Errorf("failed to execute query: %w", err)
	}
	defer rows.Close()

	if err := MapRowsToSlice(rows, dest); err != nil {
		return fmt.Errorf("failed to map rows to slice: %w", err)
	}

	return nil
}

func (p *PostgresClient) WithTransaction(action func(tx DBTransaction) error) error {
	db, err := p.connect()
	if err != nil {
		return err
	}
	defer db.Close()

	tx, err := db.Begin()
	if err != nil {
		return fmt.Errorf("failed to begin transaction: %w", err)
	}

	err = action(NewDBTransaction(tx))
	if err != nil {
		if rollbackErr := tx.Rollback(); rollbackErr != nil {
			return fmt.Errorf("failed to rollback transaction: %v (original error: %w)", rollbackErr, err)
		}
		return fmt.Errorf("transaction failed: %w", err)
	}

	if err := tx.Commit(); err != nil {
		return fmt.Errorf("failed to commit transaction: %w", err)
	}
	return nil
}
