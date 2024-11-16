package postgres

import (
	"context"
	"fmt"

	"github.com/jackc/pgx/v4"
)

var db *pgx.Conn

func NewClient() (*pgx.Conn, error) {
	var err error
	db, err = pgx.Connect(context.Background(), "postgresql://user:password@localhost:5432/mydb")
	if err != nil {
		return nil, fmt.Errorf("unable to connect to database: %w", err)
	}
	return db, nil
}
