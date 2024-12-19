FROM debian:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    openssl \
    wget \
    ca-certificates \
    bash \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN wget https://github.com/FiloSottile/mkcert/releases/download/v1.4.4/mkcert-v1.4.4-linux-amd64 -O /usr/local/bin/mkcert && \
    chmod +x /usr/local/bin/mkcert

RUN wget https://github.com/sigstore/cosign/releases/download/v2.0.1/cosign-linux-amd64 -O /usr/local/bin/cosign && \
    chmod +x /usr/local/bin/cosign

WORKDIR /certs

COPY ./scripts/generate-certs.sh /usr/local/bin/generate-certs.sh
RUN chmod +x /usr/local/bin/generate-certs.sh

