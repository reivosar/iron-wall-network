#!/bin/bash

echo "Removing all containers..."
docker rm -f $(docker ps -aq)

echo "Removing all images..."
docker rmi -f $(docker images -aq)

echo "Removing all volumes..."
docker volume rm $(docker volume ls -q)

echo "Removing all networks..."
docker network rm $(docker network ls -q)

echo "Pruning system..."
docker system prune -af --volumes

echo "Docker reset complete!"
