ARG TRIVYSCAN_VERSION
FROM aquasec/trivy:${TRIVYSCAN_VERSION}

WORKDIR /app

RUN apk add --no-cache docker-cli

COPY ./scripts/scan.sh /app/scan.sh
RUN chmod +x /app/scan.sh && ls -l /app/scan.sh

VOLUME ["/reports"]

ENTRYPOINT ["/bin/sh", "/app/scan.sh"]