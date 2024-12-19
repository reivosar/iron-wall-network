#!/bin/sh

SSL_CERTS_DIR="/certs/ssl"
SSL_CERT_FILE="$SSL_CERTS_DIR/nginx-selfsigned.crt"
SSL_KEY_FILE="$SSL_CERTS_DIR/nginx-selfsigned.key"

COSIGN_CERTS_DIR="/certs/cosign"
COSIGN_KEY_FILE="$COSIGN_CERTS_DIR/cosign.key"
COSIGN_PUB_FILE="$COSIGN_CERTS_DIR/cosign.pub"

generate_ssl_certificate() {
    echo "Generating SSL certificates..."
    mkdir -p "$SSL_CERTS_DIR"

    if [ -f "$SSL_CERT_FILE" ]; then
        end_date=$(openssl x509 -enddate -noout -in "$SSL_CERT_FILE" | cut -d= -f2)
        end_date_epoch=$(date -d "$end_date" +%s)
        current_date_epoch=$(date +%s)

        if [ "$end_date_epoch" -gt "$current_date_epoch" ]; then
            echo "SSL certificate is still valid. No need to regenerate."
            return
        else
            echo "SSL certificate has expired. Generating a new one..."
        fi
    else
        echo "SSL certificate not found. Generating a new one..."
    fi

    mkcert -cert-file "$SSL_CERT_FILE" -key-file "$SSL_KEY_FILE" localhost
    echo "SSL certificates have been generated."
}

generate_cosign_keys() {
    echo "Generating Cosign signing keys..."
    mkdir -p "$COSIGN_CERTS_DIR"

    if [ -f "$COSIGN_KEY_FILE" ] && [ -f "$COSIGN_PUB_FILE" ]; then
        echo "Cosign keys already exist. Skipping generation."
        return
    fi

    COSIGN_PASSWORD="" cosign generate-key-pair \
        --output-key-prefix "$COSIGN_CERTS_DIR/cosign"

    if [ ! -f "$COSIGN_KEY_FILE" ] || [ ! -f "$COSIGN_PUB_FILE" ]; then
        echo "Error: Failed to generate Cosign signing keys."
        exit 1
    fi

    echo "Cosign signing keys have been generated."
    echo "Key: $COSIGN_KEY_FILE"
    echo "Public Key: $COSIGN_PUB_FILE"
}

main() {
    echo "===== SSL certificate generate ====="
    generate_ssl_certificate
    echo "SSL certificate setup completed."

    echo "===== Cosign certificate generate ====="
    generate_cosign_keys
    echo "Cosign certificate setup completed."
}

main
