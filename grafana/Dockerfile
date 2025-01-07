ARG GRAFANA_VERSION
FROM grafana/grafana:${GRAFANA_VERSION}

ENV DOMAIN=localhost
ENV ROOT_URL=%(protocol)s://%(domain)s:%(http_port)s/grafana/
ENV HTTP_PORT=3000

USER root

RUN sed -i "s/^;domain = .*/domain = ${DOMAIN}/" /etc/grafana/grafana.ini && \
    sed -i "s|^;root_url = .*|root_url = ${ROOT_URL}|" /etc/grafana/grafana.ini && \
    sed -i "s/^;http_port = .*/http_port = ${HTTP_PORT}/" /etc/grafana/grafana.ini && \
    sed -i "s/^;serve_from_sub_path = .*/serve_from_sub_path = true/" /etc/grafana/grafana.ini

COPY datasources.yml /etc/grafana/provisioning/datasources/

USER grafana
