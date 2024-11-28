package main

import (
	"log"
	"strings"
	"sync"

	"backend-event-streamer/internal/app"
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
		go app.ProcessStreamGroup(streamGroup, &wg)
	}

	wg.Wait()
}
