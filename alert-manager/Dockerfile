
ARG ALERT_MANAGER_VERSION
FROM prom/alertmanager:${ALERT_MANAGER_VERSION}

WORKDIR /etc/alertmanager

COPY ./config.yaml /etc/alertmanager/config.yaml