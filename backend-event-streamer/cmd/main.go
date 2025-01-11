package main

import (
	"log"
	"strings"
	"sync"

	"backend-event-streamer/internal/app"
	"backend-event-streamer/internal/infrastructure/db"
	"backend-event-streamer/internal/infrastructure/redis"
	"backend-event-streamer/pkg/env"
)

func main() {
	streamGroupsEnv := env.GetEnv("STREAM_GROUPS", "account-events:account-event-group")
	if streamGroupsEnv == "" {
		log.Fatal("STREAM_GROUPS environment variable is not set")
	}
	streamGroups := strings.Split(streamGroupsEnv, ",")

	var wg sync.WaitGroup

	for _, streamGroup := range streamGroups {
		wg.Add(1)
		handler := app.NewEventStreamHandler(app.NewEventRepository(db.NewDBClient()))
		redisClient := redis.NewClient()
		eventStreamer := app.NewEventStreamer(handler, redisClient)
		go eventStreamer.ProcessStreamGroup(streamGroup, &wg)
	}

	wg.Wait()
}
