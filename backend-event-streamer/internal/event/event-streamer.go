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

				messageData, ok := message.Values["message"].(string)
				if !ok {
					log.Printf("Invalid message data for Event ID: %s", eventID)
					continue
				}

				fmt.Printf("Stream Group: %s, Event ID: %s, Event Data: %s\n", groupName, eventID, messageData)
			}
		}

		time.Sleep(1 * time.Second)
	}
}
