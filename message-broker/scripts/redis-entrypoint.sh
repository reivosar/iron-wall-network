#!/bin/bash

echo "Starting redis-server"
redis-server &

until redis-cli ping | grep -q "PONG"; do
  echo "Waiting for Redis to start..."
  sleep 1
done

echo "Redis is up and running!"

if [ -n "$STREAM_GROUPS" ]; then
  IFS=','
  for stream_group in $STREAM_GROUPS; do
    IFS=':' read -r stream group <<< "$stream_group"
    redis-cli XGROUP CREATE "$stream" "$group" $ MKSTREAM || echo "Consumer group for $stream already exists"
  done
fi

redis-cli XGROUP CREATE failed-events failed-events-group $ MKSTREAM || echo "Consumer group for $stream already exists"

tail -f /dev/null
