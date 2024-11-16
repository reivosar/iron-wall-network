#!/bin/bash

# Start Redis in the background with default settings (no config file)
redis-server

# Check if STREAM_GROUPS environment variable is set
if [ -n "$STREAM_GROUPS" ]; then
  IFS=','
  for stream_group in $STREAM_GROUPS; do
    IFS=':' read -r stream group <<< "$stream_group"
    # Create the consumer group if it doesn't exist
    redis-cli XGROUP CREATE "$stream" "$group" $ "MKSTREAM" || echo "Consumer group for $stream already exists"
  done
fi