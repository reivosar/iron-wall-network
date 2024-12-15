FROM sonarqube:community

USER root

COPY ./conf/sonar.properties /opt/sonarqube/conf/sonar.properties

ARG SONARQUBE_PORT
ARG SONARQUBE_DB_HOST
ARG SONARQUBE_DB_PORT
ARG SONARQUBE_DB_NAME
ARG SONARQUBE_DB_USER
ARG SONARQUBE_DB_PASSWORD

ENV SONARQUBE_PORT=${SONARQUBE_PORT}
ENV SONARQUBE_DB_HOST=${SONARQUBE_DB_HOST}
ENV SONARQUBE_DB_PORT=${SONARQUBE_DB_PORT}
ENV SONARQUBE_DB_NAME=${SONARQUBE_DB_NAME} 
ENV SONARQUBE_DB_USER=${SONARQUBE_DB_USER} 
ENV SONARQUBE_DB_PASSWORD=${SONARQUBE_DB_PASSWORD} 

RUN apt-get update && \
    apt-get install -y gettext && \
    find /opt/sonarqube/conf/ -type f -name "*.properties" -exec sh -c 'envsubst < "$1" > "$1.tmp" && mv "$1.tmp" "$1"' _ {} \;

USER sonarqube