FROM postgres:13

ARG SONARQUBE_DB_USER
ARG SONARQUBE_DB_PASSWORD
ARG SONARQUBE_DB_NAME

ENV SONARQUBE_DB_USER=$SONARQUBE_DB_USER
ENV SONARQUBE_DB_PASSWORD=$SONARQUBE_DB_PASSWORD
ENV SONARQUBE_DB_NAME=$SONARQUBE_DB_NAME

COPY ./migrations /docker-entrypoint-initdb.d/

COPY ./scripts/pgc.sh /pgc.sh
RUN chmod +x /pgc.sh

RUN apt-get update && \
    apt-get install -y gettext && \
    find /docker-entrypoint-initdb.d/ -type f -name "*.sql" -exec sh -c 'envsubst < "$1" > "$1.tmp" && mv "$1.tmp" "$1"' _ {} \;
