FROM postgres:13

ARG BACKEND_DB_USER
ARG BACKEND_DB_PASSWORD
ARG BACKEND_DB_NAME
ARG PASSWORD_SECRET_KEY
ARG AUTH_KEY_SECRET_KEY

ENV POSTGRES_USER=$BACKEND_DB_USER
ENV POSTGRES_PASSWORD=$BACKEND_DB_PASSWORD
ENV POSTGRES_DB=$BACKEND_DB_NAME
ENV PASSWORD_SECRET_KEY=$PASSWORD_SECRET_KEY
ENV AUTH_KEY_SECRET_KEY=$AUTH_KEY_SECRET_KEY

COPY ./migrations /docker-entrypoint-initdb.d/

RUN apt-get update && \
    apt-get install -y gettext && \
    find /docker-entrypoint-initdb.d/ -type f -name "*.sql" -exec sh -c 'envsubst < "$1" > "$1.tmp" && mv "$1.tmp" "$1"' _ {} \;
