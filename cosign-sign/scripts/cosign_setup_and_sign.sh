#!/bin/bash

COSIGN_CERTS_DIR="/certs/cosign"
COSIGN_TARGET_DIR="/keys"
ARTIFACTS=("reverse-proxy" "backend-api" "backend-event-streamer" "alert-manager")
ARTIFACT_DIR="/artifacts"
SIGNED_ARTIFACT_DIR="/artifacts/signed"

setup_cosign_keys() {
    echo "Setting up Cosign keys..."
    mkdir -p "$COSIGN_TARGET_DIR"

    if [ -f "$COSIGN_CERTS_DIR/cosign.key" ] && [ -f "$COSIGN_CERTS_DIR/cosign.pub" ]; then
        cp "$COSIGN_CERTS_DIR/cosign.key" "$COSIGN_TARGET_DIR/cosign.key"
        cp "$COSIGN_CERTS_DIR/cosign.pub" "$COSIGN_TARGET_DIR/cosign.pub"
        echo "Cosign keys successfully set up."
    else
        echo "Error: Cosign keys not found in $COSIGN_CERTS_DIR."
        exit 1
    fi

    echo "Cosign key setup completed."
}

generate_artifacts() {
    echo "Generating artifacts from Docker images..."
    mkdir -p "$ARTIFACT_DIR"

    for artifact_name in "${ARTIFACTS[@]}"; do
        image_name="iron-wall-network-${artifact_name}"
        artifact_path="$ARTIFACT_DIR/iron-wall-network-$artifact_name.tar.gz"

        if ! docker images --format "{{.Repository}}" | grep -q "^$image_name$"; then
            echo "Warning: Docker image not found for $artifact_name. Skipping."
            continue
        fi

        docker save "$image_name" | gzip > "$artifact_path"

        if [ $? -ne 0 ]; then
            echo "Error: Failed to generate artifact for $artifact_name."
            exit 1
        fi

        echo "Artifact generated: $artifact_path"
    done

    echo "Artifact generation completed."
}

sign_specific_artifacts() {
    echo "Signing specified artifacts..."
    mkdir -p "$SIGNED_ARTIFACT_DIR"

    for artifact_name in "${ARTIFACTS[@]}"; do
        artifact_path="$ARTIFACT_DIR/iron-wall-network-$artifact_name.tar.gz"
        signed_artifact_path="$SIGNED_ARTIFACT_DIR/iron-wall-network-$artifact_name.tar.gz.sig"

        if [ ! -f "$artifact_path" ]; then
            echo "Warning: Artifact not found: $artifact_path. Skipping."
            continue
        fi

        COSIGN_PASSWORD="" cosign sign-blob --yes --key "$COSIGN_TARGET_DIR/cosign.key" "$artifact_path" > "$signed_artifact_path"

        if [ $? -ne 0 ]; then
            echo "Error: Failed to sign artifact $artifact_path."
            exit 1
        fi

        echo "Artifact successfully signed: $artifact_path"
        echo "Signed artifact: $signed_artifact_path"
    done
}

main() {
    echo "===== Cosign Artifact Setup and Signing Process ====="
    setup_cosign_keys
    generate_artifacts
    sign_specific_artifacts
    echo "All specified artifacts signed successfully."
}

main
