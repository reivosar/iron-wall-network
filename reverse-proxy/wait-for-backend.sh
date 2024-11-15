#!/bin/sh
until curl --silent --head --fail backend-api:8080; do
    echo "Waiting for backend-api to be available..."
    sleep 5
done

echo "Backend API is ready. Starting Nginx."
nginx -g 'daemon off;'
