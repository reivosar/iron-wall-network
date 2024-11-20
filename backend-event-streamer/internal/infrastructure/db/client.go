package db

import (
	"backend-event-streamer/pkg/env"
	"context"
	"fmt"

	"github.com/jackc/pgx/v4"
)

var db *pgx.Conn

func NewClient() (*pgx.Conn, error) {

	dbUser := env.GetEnv("BACKEND_DB_USER", "iron_wall_network_user")
	dbPassword := env.GetEnv("BACKEND_DB_PASSWORD", "password")
	dbHost := env.GetEnv("BACKEND_DB_HOST", "localhost")
	dbName := env.GetEnv("BACKEND_DB_NAME", "iron_wall_network_db")

	connStr := fmt.Sprintf("postgresql://%s:%s@%s:5432/%s", dbUser, dbPassword, dbHost, dbName)

	var err error
	db, err = pgx.Connect(context.Background(), connStr)
	if err != nil {
		return nil, fmt.Errorf("unable to connect to database: %w", err)
	}
	return db, nil
}
