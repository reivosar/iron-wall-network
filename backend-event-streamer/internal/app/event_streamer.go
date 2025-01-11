package app

import (
	"backend-event-streamer/internal/infrastructure/redis"
	"context"
	"fmt"
	"log"
	"strings"
	"sync"
	"time"
)

type EventStreamer interface {
	ProcessStreamGroup(streamGroup string, wg *sync.WaitGroup)
}

type RedisEventStreamer struct {
	h  EventStreamHandler
	rc redis.RedisClient
}

func NewEventStreamer(h EventStreamHandler, rc redis.RedisClient) EventStreamer {
	return &RedisEventStreamer{h: h, rc: rc}
}

func (es *RedisEventStreamer) ProcessStreamGroup(streamGroup string, wg *sync.WaitGroup) {
	defer wg.Done()

	parts := strings.Split(streamGroup, ":")
	if len(parts) != 2 {
		log.Printf("Invalid stream group format: %s", streamGroup)
		return
	}

	streamName, groupName := parts[0], parts[1]
	ctx := context.Background()

	for {
		result, err := es.rc.ReadStreamWithGroup(ctx, streamName, groupName)
		if err != nil {
			log.Printf("Error reading from Redis stream: %v", err)
			time.Sleep(1 * time.Second)
			continue
		}

		for _, stream := range result {
			for _, message := range stream.Messages {
				eventData, err := ToEventStreamData(message.Values)
				if err != nil {
					log.Printf("Invalid message data for Event ID: %s", message.ID)
					continue
				}

				lockKey := fmt.Sprintf("lock:%s-%s-%s", groupName, streamName, eventData.AggregateID)

				if !es.rc.Lock(ctx, lockKey) {
					continue
				}

				go func(eventData *EventStreamData, messageID string) {
					if err := es.h.Handle(eventData); err != nil {
						log.Printf("Error handling event: %v", err)

						if err := es.rc.LogFailedEvent(
							ctx,
							eventData.EventID,
							eventData.AggregateID,
							eventData.AggregateType,
							eventData.EventType,
							err.Error()); err != nil {
							log.Printf("Error logging failed event: %v", err)
						}
					}

					err := es.rc.AcknowledgeMessage(ctx, streamName, groupName, messageID)
					if err != nil {
						log.Printf("Error ACKing message: %v", err)
					}

					err = es.rc.DeleteEventAndReleaseLock(ctx, streamName, messageID, lockKey)
					if err != nil {
						log.Printf("Error deleting message: %v", err)
					}
				}(eventData, message.ID)
			}
		}

		time.Sleep(1 * time.Second)
	}
}
