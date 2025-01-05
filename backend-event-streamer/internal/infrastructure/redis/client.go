package redis

import (
	"backend-event-streamer/pkg/env"
	"context"
	"fmt"
	"log"
	"time"

	"github.com/go-redis/redis/v8"
	"github.com/google/uuid"
)

type RedisClient struct {
	Client *redis.Client
}

func NewClient() *RedisClient {
	addr := env.GetEnv("MESSAGE_BROKER_HOST", "localhost")
	port := env.GetEnvInt("MESSAGE_BROKER_PORT", 6379)
	password := env.GetEnv("MESSAGE_BROKER_PASSWORD", "")

	client := redis.NewClient(&redis.Options{
		Addr:     fmt.Sprintf("%s:%d", addr, port),
		Password: password,
		DB:       0,
	})

	return &RedisClient{
		Client: client,
	}
}

// Lock acquires a lock on a specific key
func (r *RedisClient) Lock(ctx context.Context, lockKey string) bool {
	isLocked, err := r.Client.SetNX(ctx, lockKey, "locked", 5*time.Minute).Result()
	if err != nil {
		log.Printf("Error acquiring lock for %s: %v", lockKey, err)
		return false
	}

	if !isLocked {
		log.Printf("Already locked for lockKey %s, skipping", lockKey)
		return false
	}
	return true
}

// ReadStream reads messages from the specified stream
func (r *RedisClient) ReadStream(ctx context.Context, streamName string) ([]redis.XStream, error) {
	result, err := r.Client.XRead(ctx, &redis.XReadArgs{
		Streams: []string{streamName, "0"},
		Block:   0,
		Count:   10,
	}).Result()

	if err != nil {
		return nil, err
	}

	return result, nil
}

// ReadStreamWithGroup reads messages from the specified stream within a consumer group
func (r *RedisClient) ReadStreamWithGroup(ctx context.Context, streamName, groupName string) ([]redis.XStream, error) {
	consumerName := fmt.Sprintf("%s-consumer-%s", groupName, uuid.New().String())

	result, err := r.Client.XReadGroup(ctx, &redis.XReadGroupArgs{
		Group:    groupName,
		Consumer: consumerName,
		Streams:  []string{streamName, ">"},
		Block:    0,
		Count:    10,
	}).Result()

	if err != nil {
		log.Printf("Error reading from Redis streams with group: %v", err)
		return nil, err
	}

	return result, nil
}

// Ack acknowledges a message from the stream
func (r *RedisClient) AcknowledgeMessage(ctx context.Context, streamName, groupName, messageID string) error {
	return r.Client.XAck(ctx, streamName, groupName, messageID).Err()
}

// DeleteEventAndReleaseLock deletes a message from the stream and releases the lock
func (r *RedisClient) DeleteEventAndReleaseLock(ctx context.Context, streamName, messageID, lockKey string) error {
	err := r.Client.XDel(ctx, streamName, messageID).Err()
	if err != nil {
		log.Printf("Error deleting message: %v", err)
	}

	r.Client.Del(ctx, lockKey)

	return nil
}

// LogFailedEvent logs the failed event in the Redis stream
func (r *RedisClient) LogFailedEvent(ctx context.Context, failedEventId, failedAggregateId, failedAggregateType, failedEventType, errorMessage string) error {
	_, err := r.Client.XAdd(ctx, &redis.XAddArgs{
		Stream: "failed-events",
		Values: map[string]interface{}{
			"eventId":       failedEventId,
			"aggregateId":   failedAggregateId,
			"aggregateType": failedAggregateType,
			"eventType":     failedEventType,
			"errorMessage":  errorMessage,
		},
	}).Result()

	if err != nil {
		log.Printf("Error saving failed event to Redis: %v", err)
		return err
	}

	return nil
}
