ARG BACKEND_VERSION
FROM haskell:${BACKEND_VERSION}

WORKDIR /app

COPY stack.yaml stack.yaml.lock ./
COPY *.cabal . 

RUN stack setup --install-ghc && stack build --only-dependencies

COPY . .

RUN --mount=type=cache,target=/root/.stack stack build --fast
