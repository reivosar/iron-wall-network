#!/bin/bash

# Directories
TLS_DIR="/tls"
SSL_CERTS_DIR="/certs/ssl"
REDIS_CONF_DIR="/etc/redis"

# SSL Certificate Files
CERT_FILE_NAME="redis.crt"
KEY_FILE_NAME="redis.key"
CA_CERT_FILE_NAME="ca.crt"

TLS_CERT_FILE="$SSL_CERTS_DIR/nginx-selfsigned.crt"
TLS_KEY_FILE="$SSL_CERTS_DIR/nginx-selfsigned.key"
TLS_CA_CERT_FILE="$SSL_CERTS_DIR/ca.crt"

LOG_FILE="/var/log/redis-init.log"

# Redis Configuration
REDIS_PASSWORD=${MESSAGE_BROKER_PASSWORD}
REDIS_PORT=${MESSAGE_BROKER_PORT}

log_info() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') [INFO] - $1" | tee -a "$LOG_FILE"
}

log_error() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') [ERROR] - $1" | tee -a "$LOG_FILE" >&2
}

copy_ssl_certificates() {
    log_info "Copying SSL certificates to $TLS_DIR..."
    mkdir -p "$TLS_DIR"
    cp "$TLS_CERT_FILE" "$TLS_DIR/$CERT_FILE_NAME" || log_error "Error copying TLS cert file!"
    cp "$TLS_KEY_FILE" "$TLS_DIR/$KEY_FILE_NAME" || log_error "Error copying TLS key file!"
    
    if [ -f "$TLS_CA_CERT_FILE" ]; then
        cp "$TLS_CA_CERT_FILE" "$TLS_DIR/$CA_CERT_FILE_NAME" || log_error "Error copying CA cert file!"
        log_info "CA certificate found and copied."
    else
        log_info "CA certificate not found. Skipping CA certificate setup."
    fi
    log_info "SSL certificates copied to $TLS_DIR successfully."
}

setup_redis_ssl_certificates() {
    log_info "Setting up Redis SSL certificates..."
    mkdir -p "$REDIS_CONF_DIR/certs" "$REDIS_CONF_DIR/private"
    cp "$TLS_DIR/$CERT_FILE_NAME" "$REDIS_CONF_DIR/certs/" || log_error "Error copying TLS cert file to Redis directory!"
    cp "$TLS_DIR/$KEY_FILE_NAME" "$REDIS_CONF_DIR/private/" || log_error "Error copying TLS key file to Redis directory!"
    
    if [ -f "$TLS_DIR/$CA_CERT_FILE_NAME" ]; then
        cp "$TLS_DIR/$CA_CERT_FILE_NAME" "$REDIS_CONF_DIR/certs/" || log_error "Error copying CA cert file to Redis directory!"
        log_info "CA certificate found and set up for Redis."
    else
        log_info "CA certificate not found. Skipping CA certificate setup for Redis."
    fi

    log_info "Redis SSL certificates setup completed."
}

start_redis_server() {
    log_info "Starting Redis server with TLS support and password authentication..."

    if [ ! -f "$REDIS_CONF_DIR/certs/$CERT_FILE_NAME" ] || [ ! -f "$REDIS_CONF_DIR/private/$KEY_FILE_NAME" ]; then
        log_error "TLS certificates are missing. Exiting..."
        exit 1
    fi

    REDIS_CMD="redis-server --tls-port $REDIS_PORT \
        --port 0 \
        --tls-cert-file $REDIS_CONF_DIR/certs/$CERT_FILE_NAME \
        --tls-key-file $REDIS_CONF_DIR/private/$KEY_FILE_NAME"

    if [ -f "$REDIS_CONF_DIR/certs/$CA_CERT_FILE_NAME" ]; then
        REDIS_CMD="$REDIS_CMD --tls-ca-cert-file $REDIS_CONF_DIR/certs/$CA_CERT_FILE_NAME"
    fi

    REDIS_CMD="$REDIS_CMD --requirepass $REDIS_PASSWORD"
    log_info "Redis server command: $REDIS_CMD"
    nohup $REDIS_CMD >> /var/log/redis-server.log 2>&1 &
    log_info "Redis server started successfully."
}

wait_for_redis() {
    log_info "Waiting for Redis to start..."
    export REDISCLI_AUTH=$REDIS_PASSWORD
    until redis-cli --tls \
                    --cert "$TLS_DIR/$CERT_FILE_NAME" \
                    --key "$TLS_DIR/$KEY_FILE_NAME" \
                    --cacert "$TLS_DIR/$CA_CERT_FILE_NAME" \
                    -p $REDIS_PORT \
                    ping | grep -q "PONG"; do
        sleep 1
        log_error "Redis is not yet available. Retrying..."
    done
    log_info "Redis is up and running!"
}

setup_stream_groups() {
    if [ -n "$STREAM_GROUPS" ]; then
        log_info "Setting up Redis stream groups..."
        IFS=','
        for stream_group in $STREAM_GROUPS; do
            IFS=':' read -r stream group <<< "$stream_group"
            redis-cli --tls \
                --cert "$TLS_DIR/$CERT_FILE_NAME" \
                --key "$TLS_DIR/$KEY_FILE_NAME" \
                --cacert "$TLS_DIR/$CA_CERT_FILE_NAME" \
                XGROUP CREATE "$stream" "$group" $ MKSTREAM || \
                log_info "Consumer group for $stream already exists"
        done
        log_info "Redis stream groups setup completed."
    else
        log_info "No stream groups specified."
    fi
}

start_redis_server_without_tls() {
    log_info "Starting Redis server with password authentication only..."

    REDIS_CMD="redis-server \
        --port $REDIS_PORT \
        --requirepass $REDIS_PASSWORD"
    
    log_info "Redis server command: $REDIS_CMD"
    nohup $REDIS_CMD >> /var/log/redis-server.log 2>&1 &
    log_info "Redis server started successfully."
}

wait_for_redis_without_tls() {
    log_info "Waiting for Redis to start..."
    export REDISCLI_AUTH=$REDIS_PASSWORD
    until redis-cli -p $REDIS_PORT ping | grep -q "PONG"; do
        sleep 1
        log_error "Redis is not yet available. Retrying..."
    done
    log_info "Redis is up and running!"
}

setup_stream_groups_without_tls() {
    if [ -n "$STREAM_GROUPS" ]; then
        log_info "Setting up Redis stream groups..."
        IFS=','
        for stream_group in $STREAM_GROUPS; do
            IFS=':' read -r stream group <<< "$stream_group"
            redis-cli -p $REDIS_PORT \
                XGROUP CREATE "$stream" "$group" $ MKSTREAM || \
                log_info "Consumer group for $stream already exists"
        done
        log_info "Redis stream groups setup completed."
    else
        log_info "No stream groups specified."
    fi
}

main() {
    log_info "Starting Redis initialization..."
    copy_ssl_certificates
    setup_redis_ssl_certificates
    start_redis_server
    wait_for_redis
    setup_stream_groups
    log_info "Redis initialization completed."
}

main_without_tls() {
    log_info "Starting Redis initialization..."
    start_redis_server_without_tls
    wait_for_redis_without_tls
    setup_stream_groups_without_tls
    log_info "Redis initialization completed."
}

main_without_tls
tail -f /dev/null
