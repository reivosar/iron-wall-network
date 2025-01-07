#!/bin/bash

docker compose -f docker-certificates.yml \
               -f docker-reverse-proxy.yml \
               -f docker-alerting.yml \
               -f docker-backend.yml \
               -f docker-monitoring.yml \
               -f docker-tool.yml \
               -f docker-common-settings.yml up --build
