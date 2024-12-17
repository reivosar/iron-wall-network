#!/bin/sh

SERVICES="backend-api backend-event-streamer message-broker"
REPORT_DIR="/reports"

if ! docker ps >/dev/null 2>&1; then
  echo "Error: Docker daemon is not accessible. Make sure /var/run/docker.sock is mounted."
  exit 1
fi

mkdir -p "${REPORT_DIR}"

for SERVICE in ${SERVICES}; do
  echo "Scanning ${SERVICE}..."

  IMAGE=$(docker images --format "{{.Repository}}:{{.Tag}}" | grep -E "^${SERVICE}(:|$)" | head -n 1)

  if [ -z "${IMAGE}" ]; then
    echo "Warning: No image found for service '${SERVICE}'. Skipping..."
    continue
  fi

  echo "Using image: ${IMAGE}"
  trivy image --exit-code 0 --severity CRITICAL,HIGH "${IMAGE}" -f json -o "${REPORT_DIR}/${SERVICE}-report.json"
done
