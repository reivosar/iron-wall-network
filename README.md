Here is the detailed version of the README with updated explanations for the event processing flow:

---

# Iron Wall Network

Iron Wall Network is an integrated infrastructure combining multiple components for secure, efficient backend API management, event-driven processing, monitoring, and visualization. The system is built using a Haskell backend API, Grafana for monitoring, Prometheus for metrics collection, and Nginx as a reverse proxy. It includes features for handling events, processing data, and ensuring system security.

## Table of Contents
- [Features](#features)
- [Accessing Services](#accessing-services)
- [Security Considerations](#security-considerations)
- [Event Processing Flow](#event-processing-flow)
- [Installation](#installation)
- [Environment Variables](#environment-variables)

## Features

### 1. **Backend API**  
   - **Technology**: Haskell, using the Servant library for API definition.
   - **Endpoints**: Handles various banking operations such as account creation, deposits, withdrawals, account approvals, and updates.
   - **Event-driven**: The system is built to handle events such as account creation, fund deposits, and transactions, ensuring asynchronous processing.

### 2. **Grafana**  
   - **Purpose**: Provides visualization and real-time monitoring.
   - **Access**: [https://localhost/grafana/](https://localhost/grafana/)

### 3. **Prometheus**  
   - **Purpose**: Collects metrics for monitoring and alerting.
   - **Access**: [https://localhost/prometheus/](https://localhost/prometheus/)

### 4. **Nginx Reverse Proxy**  
   - **Purpose**: Routes traffic to appropriate services, with load balancing and SSL encryption.
   - **Security**: Integrates ModSecurity for enhanced security measures and rate limiting.
   - **Access**: [https://localhost/api/](https://localhost/api/)

## Accessing Services
After starting the services, you can access them at the following endpoints:

- **API Endpoint:** [https://localhost/api/](https://localhost/api/)
- **Grafana Dashboard:** [https://localhost/grafana/](https://localhost/grafana/)
- **Prometheus Interface:** [https://localhost/prometheus/](https://localhost/prometheus/)

## Security Considerations

### 1. **ModSecurity**  
   - Web application firewall for filtering HTTP traffic and monitoring requests.

### 2. **Rate Limiting**  
   - Controls the rate of incoming requests to prevent denial-of-service (DoS) attacks.

### 3. **SSL/TLS Encryption**  
   - Ensures secure, encrypted communication between clients and servers.

## Event Processing Flow

The **event-driven** architecture ensures that significant actions (e.g., account creation, fund deposits) trigger events, which are then processed by the backend system in a well-defined manner. Events are captured, processed, and eventually acknowledged to maintain data consistency and avoid issues like double-processing.

### Event Flow Overview:

1. **Event Creation**:
   - Events like `AccountCreated` or `DepositFunds` are triggered when users interact with the API.
   - These events are serialized and written to a Redis stream. The events are typically associated with a specific **aggregate** (like `account` or `funds`), ensuring that the events are correctly captured in the context of the entity they represent.

2. **Event Processing**:
   - **Consumers** (services that process events) subscribe to Redis streams. Redis Streams provide an event-sourcing mechanism where each event is processed in order, ensuring **exactly-once** processing of each event.
   - Events are processed via Redis’ `XREADGROUP` command, which makes sure that each event is consumed by only one consumer at a time.

3. **Locking and Concurrency**:
   - To handle concurrency, the system uses **distributed locks** (via Redis) to prevent multiple consumers from processing the same event simultaneously.
   - A unique **lock key** is created based on the aggregate ID (e.g., account ID), ensuring that the same aggregate is processed sequentially by a single consumer. This guarantees that no conflicting operations (like multiple deposits to the same account) are performed at once.

4. **Event Acknowledgement and Deletion**:
   - After successfully processing an event, the consumer **acknowledges** the event by sending an `XACK` command to Redis. This ensures that the event is marked as processed and no longer in the stream.
   - The event is then **deleted** from the Redis stream using the `XDEL` command, ensuring that no further processing will occur for the same event.

5. **Error Handling and Retries**:
   - If an event fails during processing, the system retries the event based on a defined retry policy.
   - Failed events are logged and stored in a separate "failed events" table for later review, and consumers can attempt to reprocess these events manually or automatically.

### Event Flow Example:
1. **Create Account**:
   - A user submits an account creation request via the API.
   - This triggers the `AccountCreated` event.
   - The event is serialized and written to the Redis stream associated with the `account` aggregate ID.
   - The consumer processes the event by creating a new account in the system.
   - Upon successful processing, the event is acknowledged and deleted from the stream.

2. **Deposit Funds**:
   - A user deposits funds into their account, which triggers the `DepositFunds` event.
   - The system locks the account aggregate to ensure that the deposit is processed sequentially.
   - Once processed, the event is acknowledged and deleted.

3. **Event Retrying**:
   - If, for example, the `DepositFunds` event fails (due to network issues or database downtime), the system will attempt to retry processing the event.
   - The system tracks retries and will stop retrying after a predefined number of attempts, logging the failed event for manual intervention.

## Installation

### 1. Clone the Repository
```bash
git clone https://github.com/reivosar/iron-wall-network.git
cd iron-wall-network
```

### 2. Configure Environment Variables
Copy `.env.example` to `.env` and fill in the necessary values, including PostgreSQL credentials, backend, Nginx, Grafana, and Prometheus configurations.

### 3. Build and Start Services
Ensure Docker and Docker Compose are installed on your system. Then, run:
```bash
docker-compose up --build
```
This will build and start all services, including the backend API, Grafana, Prometheus, and Nginx as the reverse proxy.

## Environment Variables

Example `.env` file:
```bash
# PostgreSQL
POSTGRES_USER=                           # Database user for PostgreSQL (Sensitive)
POSTGRES_PASSWORD=                       # Password for PostgreSQL user (Sensitive)
POSTGRES_DB=iron_wall_network_db         # Database name (Not sensitive)
POSTGRES_PORT=5432                       # Port for PostgreSQL (Not sensitive)

# PostgresExporter
POSTGRES_EXPORTER_VERSION=latest         # Version of Postgres Exporter (Not sensitive)
POSTGRES_EXPORTER_PORT=9187              # Port for Postgres Exporter (Not sensitive)

# Backend
BACKEND_DB_USER=                         # Database user for backend (Sensitive)
BACKEND_DB_PASSWORD=                     # Password for backend database user (Sensitive)
BACKEND_DB_HOST=db                       # Database host (Not sensitive)
BACKEND_DB_NAME=iron_wall_network_db     # Database name for backend (Not sensitive)

# Backend-API
BACKEND_VERSION=9.2                      # Version of backend api service (Not sensitive)
BACKEND_PORT=8080                        # Backend api service port (Not sensitive)

# MssageBroker
MESSAGE_BROKER_HOST=message-broker       # Redis host (Not sensitive)
MESSAGE_BROKER_VERSION=9.2               # Version of message broker service (Not sensitive)
MESSAGE_BROKER_PORT=6379                 # Message broker serivice port (Not sensitive)
STREAM_GROUPS=account-events:account-event-group # Stream names and consumer groups, separated by commas

# Nginx
NGINX_VERSION=1.23.3                     # Nginx version (Not sensitive)
NGINX_PORT=80                            # HTTP port for Nginx (Not sensitive)
NGINX_SSL_PORT=443                       # SSL port for Nginx (Not sensitive)

# Grafana
GRAFANA_VERSION=latest                   # Grafana version (Not sensitive)
GRAFANA_PORT=3002                        # Grafana service port (Not sensitive)

# Kibana
KIBANA_VERSION=8.6.0                     # Kibana version (Not sensitive)
KIBANA_PORT=5601                         # Kibana service port (Not sensitive)

# Elasticsearch
ELASTICSEARCH_VERSION=8.6.0              # Elasticsearch version (Not sensitive)
ELASTICSEARCH_PORT=9200                  # Elasticsearch service port (Not sensitive)

# Logstash
LOGSTASH_VERSION=8.6.0                   # Logstash version (Not sensitive)
LOGSTASH_PORT=5044                       # Logstash service port (Not sensitive)

# Prometheus
PROMETHEUS_VERSION=latest                # Prometheus version (Not sensitive)
PROMETHEUS_PORT=9090                     # Prometheus service port (Not sensitive)
```

**Note:** Sensitive information like passwords should be masked or securely managed.

---

By following these steps, you can set up a secure and efficient environment for managing multiple services, with Nginx serving as a reverse proxy to manage and route traffic.