package event

import (
	"context"
	"fmt"
	"log"
	"strings"
	"sync"
	"time"

	"backend-event-streamer/internal/redis"
)

func ProcessStreamGroup(streamGroup string, wg *sync.WaitGroup) {
	defer wg.Done()

	parts := strings.Split(streamGroup, ":")
	if len(parts) != 2 {
		log.Printf("Invalid stream group format: %s", streamGroup)
		return
	}

	streamName, groupName := parts[0], parts[1]

	redisClient := redis.NewClient()
	ctx := context.Background()

	for {
		result, err := redisClient.ReadStreamWithGroup(ctx, streamName, groupName)
		if err != nil {
			log.Printf("Error reading from Redis stream: %v", err)
			time.Sleep(1 * time.Second)
			continue
		}

		for _, stream := range result {
			for _, message := range stream.Messages {
				eventData, err := ToEvent(message.Values)
				if err != nil {
					log.Printf("Invalid message data for Event ID: %s", message.ID)
					continue
				}

				lockKey := fmt.Sprintf("lock:%s-%s-%s", groupName, streamName, eventData.AggregateID)

				if !redisClient.Lock(ctx, lockKey) {
					continue
				}

				go func(eventData *StreamEvent, messageID string) {
					if err := Handle(eventData); err != nil {
						log.Printf("Error handling event: %v", err)

						if err := redisClient.LogFailedEvent(
							ctx,
							eventData.EventID,
							eventData.AggregateID,
							eventData.AggregateType,
							eventData.EventType,
							err.Error()); err != nil {
							log.Printf("Error logging failed event: %v", err)
						}
					}

					err := redisClient.AcknowledgeMessage(ctx, streamName, groupName, messageID)
					if err != nil {
						log.Printf("Error ACKing message: %v", err)
					}

					err = redisClient.DeleteEventAndReleaseLock(ctx, streamName, messageID, lockKey)
					if err != nil {
						log.Printf("Error deleting message: %v", err)
					}
				}(eventData, message.ID)
			}
		}

		time.Sleep(1 * time.Second)
	}
}
