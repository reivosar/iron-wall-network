ARG EVENT_STREAMER_VERSION
FROM golang:${EVENT_STREAMER_VERSION} as builder

COPY . /build/
WORKDIR /build

RUN go mod download
RUN go mod tidy

RUN CGO_ENABLED=0 go build -a -installsuffix cgo --ldflags "-s -w" -o /build/cmd/main ./cmd

FROM alpine:3.15.4

WORKDIR /app

RUN adduser -S -D -H -h /app gouser
USER gouser

COPY --from=builder /build/cmd/main /app/

ENTRYPOINT ./main
