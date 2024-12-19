ARG MESSAGE_BROKER_VERSION
FROM redis:${MESSAGE_BROKER_VERSION}

COPY ./scripts/redis-entrypoint.sh /usr/local/bin/redis-entrypoint.sh
RUN chmod +x /usr/local/bin/redis-entrypoint.sh

ENTRYPOINT ["/usr/local/bin/redis-entrypoint.sh"]
