#!/bin/bash

# Load sensitive data from .env file if it exists
if [ -f .env ]; then
  export $(grep -v '^#' .env | xargs)
else
  echo "Error: .env file not found. Please create one with SONARQUBE_HOST, SONARQUBE_USER, and SONARQUBE_PASSWORD."
  exit 1
fi

# Check for mandatory variables
if [ -z "$SONARQUBE_HOST" ] || [ -z "$SONARQUBE_USER" ] || [ -z "$SONARQUBE_PASSWORD" ]; then
  echo "Error: SONARQUBE_HOST, SONARQUBE_USER, and SONARQUBE_PASSWORD must be set in .env file."
  exit 1
fi

echo "USE SONARQUBE_HOST: $SONARQUBE_HOST"
echo "USE SONARQUBE_USER: $SONARQUBE_USER"

docker_up() {
  echo "Checking if SonarQube container is already running..."

  local container_name="sonarqube"

  if docker ps --filter "name=$container_name" --format "{{.Names}}" | grep -q "$container_name"; then
    echo "SonarQube container is already running."

    local health_status
    health_status=$(docker inspect --format "{{.State.Health.Status}}" "$container_name" 2>/dev/null || echo "none")

    if [ "$health_status" = "healthy" ]; then
      echo "SonarQube container is healthy. Skipping restart."
      return 0
    else
      echo "SonarQube container is running but not healthy. Restarting..."
      docker compose restart "$container_name"
    fi
  else
    echo "SonarQube container is not running. Starting it..."
    docker compose up --build -d
  fi

  echo "Waiting for SonarQube to be ready..."

  local retries=30
  local count=0
  while [ $count -lt $retries ]; do
    local status
    status=$(curl -s -k "$SONARQUBE_HOST/api/system/status" | jq -r '.status' 2>/dev/null)

    if [ "$status" = "UP" ]; then
      echo "SonarQube is up and running!"
      break
    fi

    echo "Waiting for SonarQube to become ready... (Attempt $((count+1))/$retries)"
    sleep 2
    count=$((count + 1))
  done

  if [ $count -eq $retries ]; then
    echo "Error: SonarQube did not start within the expected time."
    exit 1
  fi

  echo "Verifying container status..."
  docker compose ps
}

install_sonar_scanner() {
  echo "Checking Sonar Scanner installation..."
  if ! [ -x "$(command -v sonar-scanner)" ]; then
    echo "Installing Sonar Scanner..."
    wget https://binaries.sonarsource.com/Distribution/sonar-scanner-cli/sonar-scanner-cli-4.8.0.2856-macosx.zip
    unzip sonar-scanner-cli-4.8.0.2856-macosx.zip -d /usr/local/bin
    export PATH=$PATH:/usr/local/bin/sonar-scanner-4.8.0.2856-macosx/bin
  else
    echo "Sonar Scanner is already installed."
  fi
}

change_default_admin_password() {
  local new_password="$SONARQUBE_PASSWORD" 

  echo "Checking if admin password is still default..."

  local response
  response=$(curl -s -k -u "$SONARQUBE_USER:admin" "$SONARQUBE_HOST/api/authentication/validate")

  local is_valid
  is_valid=$(echo "$response" | jq -r '.valid')

  if [ "$is_valid" = "false" ]; then
    echo "Admin password has already been changed. Skipping password update."
    return 0
  elif [ "$is_valid" = "true" ]; then
    echo "Admin password is still the default. Proceeding to change..."
  else
    echo "Error: Unexpected response received: $response"
    exit 1
  fi

  response=$(curl -s -k -u "admin:admin" -X POST \
    "$SONARQUBE_HOST/api/users/change_password" \
    -d "login=admin" \
    -d "previousPassword=admin" \
    -d "password=$new_password")

  local http_status
  http_status=$(echo "$response" | grep "HTTP_STATUS" | cut -d':' -f2 | tr -d '\n')
  local body
  body=$(echo "$response" | sed '/HTTP_STATUS:/d')

  if [ "$http_status" -ne 204 ]; then
    echo "Error: Failed to change admin password. HTTP Status: $http_status. Response: $body"
    exit 1
  fi

  echo "Admin password changed successfully."
}

check_project_existence() {
  local project_key=$1
  echo "Checking if project '$project_key' exists..."

  local response
  response=$(curl -s -k -w "\nHTTP_STATUS:%{http_code}" -u "$SONARQUBE_USER:$SONARQUBE_PASSWORD" \
    "$SONARQUBE_HOST/api/projects/search?projects=$project_key")
  
  local http_status
  http_status=$(echo "$response" | grep "HTTP_STATUS" | cut -d':' -f2 | tr -d '\n')
  local body
  body=$(echo "$response" | sed '/HTTP_STATUS:/d')

  if [ "$http_status" -ne 200 ]; then
    echo "Error: Failed to check project existence. HTTP Status: $http_status. Response: $body"
    return 1
  fi

  if echo "$body" | jq -e '.components | length > 0' >/dev/null 2>&1; then
    echo "Project '$project_key' exists."
    return 0
  else
    echo "Project '$project_key' does not exist."
    return 1
  fi
}

create_project() {
  local project_key=$1
  echo "Creating project '$project_key'..."

  local response
  response=$(curl -s -k -X POST -u "$SONARQUBE_USER:$SONARQUBE_PASSWORD" \
    -H "Content-Type: application/x-www-form-urlencoded" \
    "$SONARQUBE_HOST/api/projects/create" \
    -d "name=$project_key" -d "project=$project_key" \
    -w "\nHTTP_STATUS:%{http_code}")

  local http_status
  http_status=$(echo "$response" | grep "HTTP_STATUS" | cut -d':' -f2 | tr -d '\n')
  local body
  body=$(echo "$response" | sed '/HTTP_STATUS:/d')

  if [ "$http_status" -ne 200 ]; then
    echo "Error: Failed to create project '$project_key'. HTTP Status: $http_status. Response: $body"
    exit 1
  fi

  echo "Project '$project_key' created successfully."
}

create_project_token() {
  local project_key=$1
  local token_name="${project_key}_token"
  echo "Managing token for project '$project_key'..."

  local existing_tokens_response
  existing_tokens_response=$(curl -s -k -u "$SONARQUBE_USER:$SONARQUBE_PASSWORD" -X GET "$SONARQUBE_HOST/api/user_tokens/search")
  if echo "$existing_tokens_response" | jq -e ".userTokens[] | select(.name == \"$token_name\")" >/dev/null; then
    echo "Token '$token_name' already exists. Deleting it..."
    curl -s -k -u "$SONARQUBE_USER:$SONARQUBE_PASSWORD" -X POST \
      "$SONARQUBE_HOST/api/user_tokens/revoke" -d "name=$token_name" >/dev/null
    echo "Existing token '$token_name' deleted."
  fi

  echo "Creating new token for project '$project_key'..."
  local response
  response=$(curl -s -k -u "$SONARQUBE_USER:$SONARQUBE_PASSWORD" -X POST \
    "$SONARQUBE_HOST/api/user_tokens/generate" \
    -d "name=$token_name" \
    -d "type=PROJECT_ANALYSIS_TOKEN" \
    -d "projectKey=$project_key")

  echo "API response for token creation: $response"

  local token
  token=$(echo "$response" | jq -r '.token' | tr -d '\n' | tr -d '\r')

  if [ -z "$token" ] || [ "$token" == "null" ]; then
    echo "Error: Failed to create token for project '$project_key'. Response: $response"
    exit 1
  fi

  echo "Token created: $token"
  export SONAR_TOKEN="$token"
}

prepare_and_scan_project() {
  local project_key=$1
  local source_dir=$2

  export SONAR_TOKEN=""

  if ! check_project_existence "$project_key"; then
    echo "Project '$project_key' does not exist. Creating..."
    create_project "$project_key"
    echo "Project '$project_key' created successfully."
  else
    echo "Project '$project_key' already exists."
  fi

  create_project_token "$project_key"

  if [ -z "$SONAR_TOKEN" ] || [ "$SONAR_TOKEN" == "null" ]; then
    echo "Error: Failed to retrieve token for project '$project_key'."
    exit 1
  fi

  echo "Starting SonarQube scan for project '$project_key'..."

  pushd "$source_dir" > /dev/null

  sonar-scanner \
    -Dsonar.projectKey="$project_key" \
    -Dsonar.sources="$source_dir" \
    -Dsonar.host.url="$SONARQUBE_HOST" \
    -Dsonar.token="$SONAR_TOKEN"

  popd > /dev/null

  if [ $? -eq 0 ]; then
    echo "Scan completed successfully for project: $project_key"
    echo "Check the results at: $SONARQUBE_HOST/dashboard?id=$project_key"
  else
    echo "Error: Scan failed for project '$project_key'."
    exit 1
  fi
}

main() {
  install_prerequisites
  install_sonar_scanner
  docker_up
  change_default_admin_password
  prepare_and_scan_project "iron_wall_network_backend_api" "../backend-api"
  prepare_and_scan_project "iron_wall_network_backend_event_streamer" "../backend-event-streamer"
  echo "All scans completed."
}

main
