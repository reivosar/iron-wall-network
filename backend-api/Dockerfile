ARG BACKEND_API_VERSION
FROM haskell:${BACKEND_API_VERSION}

RUN apt-get update && apt-get install -y \
    libpq-dev \           
    zlib1g-dev \          
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY stack.yaml stack.yaml.lock ./
COPY *.cabal . 

RUN stack setup --install-ghc
RUN stack build --only-dependencies --fast --no-haddock-deps

COPY . .

RUN --mount=type=cache,target=/root/.stack stack build --fast