#!/bin/sh

SSL_CERTS_DIR="/certs/ssl"
MAX_RETRIES=10
RETRY_INTERVAL=5
SSL_TARGET_DIR="/etc/ssl"

wait_for_ssl_certificates() {
    echo "Waiting for SSL certificates to be generated..."
    retry=0
    while [ $retry -lt $MAX_RETRIES ]; do
        if [ -f "$SSL_CERTS_DIR/nginx-selfsigned.crt" ] && [ -f "$SSL_CERTS_DIR/nginx-selfsigned.key" ]; then
            echo "SSL certificates found. Proceeding with setup."
            return
        fi
        echo "SSL certificates not found. Retrying in $RETRY_INTERVAL seconds..."
        retry=$((retry + 1))
        sleep $RETRY_INTERVAL
    done

    echo "Error: SSL certificates were not generated in time."
    exit 1
}

setup_ssl_certificates() {
    echo "Copying SSL certificates to target directories..."
    mkdir -p "$SSL_TARGET_DIR/certs" "$SSL_TARGET_DIR/private"
    cp "$SSL_CERTS_DIR/nginx-selfsigned.crt" "$SSL_TARGET_DIR/certs/"
    cp "$SSL_CERTS_DIR/nginx-selfsigned.key" "$SSL_TARGET_DIR/private/"
    echo "SSL certificates successfully set up."
}

start_nginx() {
    echo "Starting Nginx..."
    nginx -g 'daemon off;'
}

main() {
    wait_for_ssl_certificates
    setup_ssl_certificates
    start_nginx
}

main
