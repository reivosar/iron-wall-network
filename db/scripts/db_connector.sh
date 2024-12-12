#!/bin/bash

if [[ -z "$POSTGRES_USER" || -z "$POSTGRES_PASSWORD" || -z "$POSTGRES_DB" ]]; then
    echo "Error: Environment variables POSTGRES_USER, POSTGRES_PASSWORD, and POSTGRES_DB must be set."
    exit 1
fi

POSTGRES_HOST=${POSTGRES_HOST:-"localhost"}
POSTGRES_PORT=${POSTGRES_PORT:-"5432"}

echo "Connecting to PostgreSQL database '$POSTGRES_DB' as user '$POSTGRES_USER' on host '$POSTGRES_HOST:$POSTGRES_PORT'..."
PGPASSWORD=$POSTGRES_PASSWORD psql -h $POSTGRES_HOST -p $POSTGRES_PORT -U $POSTGRES_USER -d $POSTGRES_DB
