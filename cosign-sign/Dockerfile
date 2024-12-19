FROM debian:latest

RUN apt-get update && apt-get install -y --no-install-recommends \
    bash \
    wget \
    ca-certificates \
    curl \
    gnupg \
    lsb-release \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg && \
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/debian $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list && \
    apt-get update && apt-get install -y docker-ce-cli && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

ARG COSIGN_VERSION
RUN wget https://github.com/sigstore/cosign/releases/download/v${COSIGN_VERSION}/cosign-linux-amd64 -O /usr/local/bin/cosign && \
    chmod +x /usr/local/bin/cosign

WORKDIR /app

COPY ./scripts/cosign_setup_and_sign.sh /cosign_setup_and_sign.sh
RUN chmod +x /cosign_setup_and_sign.sh

ENTRYPOINT ["/cosign_setup_and_sign.sh"]
