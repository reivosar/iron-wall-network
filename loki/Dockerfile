ARG LOKI_VERSION
FROM grafana/loki:${LOKI_VERSION}

RUN mkdir -p /etc/loki/rules/fake

COPY loki-config.yaml /etc/loki/loki-config.yaml
COPY nginx-error-rules.yaml /etc/loki/rules/fake/rules.yml