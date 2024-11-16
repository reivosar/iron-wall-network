package redis

import (
	"backend-event-streamer/pkg/env"
	"context"
	"fmt"
	"log"

	"github.com/go-redis/redis/v8"
)

func NewClient() *redis.Client {
	addr := env.GetEnv("MESSAGE_BROKER_HOST", "localhost")
	port := env.GetEnvInt("MESSAGE_BROKER_PORT", 6379)

	rdb := redis.NewClient(&redis.Options{
		Addr: fmt.Sprintf("%s:%d", addr, port),
		DB:   0,
	})
	return rdb
}

func ReadStream(ctx context.Context, rdb *redis.Client, streamName string) ([]redis.XStream, error) {
	result, err := rdb.XRead(ctx, &redis.XReadArgs{
		Streams: []string{streamName, "0"},
		Block:   0,
		Count:   10,
	}).Result()

	if err != nil {
		return nil, err
	}

	return result, nil
}

// ReadStreamWithGroup reads messages from a Redis stream using a consumer group
func ReadStreamWithGroup(ctx context.Context, rdb *redis.Client, streamName, groupName string) ([]redis.XStream, error) {
	// XReadGroup will read from the stream as part of a consumer group.
	result, err := rdb.XReadGroup(ctx, &redis.XReadGroupArgs{
		Group:    groupName, // Consumer group
		Consumer: groupName, // Consumer name (same as group for simplicity)
		Streams:  []string{streamName, ">"},
		Block:    0,  // Block indefinitely until new data is available
		Count:    10, // Read a maximum of 10 messages at once
	}).Result()

	if err != nil {
		log.Printf("Error reading from Redis streams with group: %v", err)
		return nil, err
	}

	return result, nil
}
