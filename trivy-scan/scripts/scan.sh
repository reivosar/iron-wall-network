#!/bin/sh

SERVICES="backend-api backend-event-streamer message-broker"
REPORT_DIR="/reports"
IMAGE_PREFIX="iron-wall-network-"

if ! docker ps >/dev/null 2>&1; then
  echo "Error: Docker daemon is not accessible. Make sure /var/run/docker.sock is mounted."
  exit 1
fi

TIMESTAMP=$(date "+%Y%m%d-%H%M%S")
TIMESTAMP_DIR="${REPORT_DIR}/${TIMESTAMP}"
mkdir -p "${TIMESTAMP_DIR}"

for SERVICE in ${SERVICES}; do
  echo "Scanning ${SERVICE}..."

  IMAGE=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep -E "^${IMAGE_PREFIX}${SERVICE}(:|$)" | head -n 1)

  echo "DEBUG: Matching image for service '${SERVICE}': ${IMAGE}"

  if [ -z "${IMAGE}" ]; then
    echo "Warning: No image found for service '${SERVICE}'. Skipping..."
    continue
  fi

  echo "Using image: ${IMAGE}"
  trivy image --exit-code 0 --severity CRITICAL,HIGH "${IMAGE}" -f json -o "${TIMESTAMP_DIR}/${SERVICE}-report.json"

  if [ $? -eq 0 ]; then
    echo "Scan completed for ${SERVICE}. Report saved to ${TIMESTAMP_DIR}/${SERVICE}-report.json"
  else
    echo "Error: Scan failed for ${SERVICE}"
  fi
done

echo "All scans completed."
