package event

import (
	"context"
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

	streamName, _ := parts[0], parts[1]

	rdb := redis.NewClient()
	ctx := context.Background()

	for {
		result, err := redis.ReadStream(ctx, rdb, streamName)
		if err != nil {
			log.Printf("Error reading from Redis stream: %v", err)
			time.Sleep(1 * time.Second)
			continue
		}

		for _, stream := range result {
			for _, message := range stream.Messages {
				eventID := message.ID
				eventData, err := ParseMessage(eventID, message.Values)
				if err != nil {
					log.Printf("Invalid message data for Event ID: %s", eventID)
					continue
				}

				go Handle(eventID, eventData)
			}
		}

		time.Sleep(1 * time.Second)
	}
}
