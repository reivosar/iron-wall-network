#!/bin/bash

POSTGRES_HOST=db
POSTGRES_PORT=5432

connect_to_db() {
    local db_name=$1
    local db_user=$2
    echo "Connecting to PostgreSQL database '$db_name' as user '$db_user' on host '$POSTGRES_HOST:$POSTGRES_PORT'..."
    PGPASSWORD=$POSTGRES_PASSWORD psql -h $POSTGRES_HOST -p $POSTGRES_PORT -U $db_user -d $db_name
}

echo "Select the database to connect to:"
echo "1) $BACKEND_DB_NAME"
echo "2) $SONARQUBE_DB_NAME"
read -p "Enter the number of the database: " choice

case $choice in
    1)
        connect_to_db "$BACKEND_DB_NAME" "$BACKEND_DB_USER"
        ;;
    2)
        connect_to_db "$SONARQUBE_DB_NAME" "$SONARQUBE_DB_USER"
        ;;
    *)
        echo "Invalid choice. Exiting."
        exit 1
        ;;
esac
