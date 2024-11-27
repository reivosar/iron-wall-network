Here is the updated version of the README for the `Iron Wall Network` repository with detailed event processing and related explanations:

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

### Event Details:

#### **Event: AccountCreated**
   - **Tables Updated**: 
     - `bank_users`
     - `bank_user_profiles`
     - `bank_accounts`
     - `pending_account`
   - **Description**: This event is triggered when a user account is created. It inserts the necessary data into the tables representing the user, profile, account, and sets the account to be pending.

#### **Event: AccountApproved**
   - **Tables Updated**: 
     - `active_accounts`
   - **Description**: This event is triggered when an account is approved. It moves the account from pending to active.

#### **Event: AccountPended**
   - **Tables Updated**: 
     - `pending_account`
   - **Description**: This event is triggered when an account is pended, indicating a state where the account is not active.

#### **Event: AccountSuspended**
   - **Tables Updated**: 
     - `suspend_accounts`
   - **Description**: This event is triggered when an account is suspended, and the suspension reason is stored.

#### **Event: AccountActivated**
   - **Tables Updated**: 
     - `pending_account` (removes the account)
     - `active_accounts` (sets the account as active)
   - **Description**: This event is triggered when a pending account is activated.

#### **Event: FundsDeposited**
   - **Tables Updated**: 
     - `bank_balance`
     - `bank_balance_history`
   - **Description**: This event is triggered when funds are deposited into an account. It updates the balance and records the transaction in the balance history.

#### **Event: FundsWithdrawn**
   - **Tables Updated**: 
     - `bank_balance`
     - `bank_balance_history`
   - **Description**: This event is triggered when funds are withdrawn from an account. It updates the balance and records the transaction in the balance history.

#### **Event: UserContactInfoUpserted**
   - **Tables Updated**: 
     - `bank_user_contacts`
   - **Description**: This event is triggered when the user’s contact info (email) is created or updated.

#### **Event: PhoneNumberUpserted**
   - **Tables Updated**: 
     - `bank_user_phone_numbers`
   - **Description**: This event is triggered when the user's phone number is added or updated.

#### **Event: AddressUpserted**
   - **Tables Updated**: 
     - `bank_user_addresses`
   - **Description**: This event is triggered when the user's address is added or updated.

#### **Event: EmergencyContactUpserted**
   - **Tables Updated**: 
     - `bank_user_emergency_contacts`
   - **Description**: This event is triggered when the user's emergency contact information is added or updated.

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
POSTGRES_USER=postgres
POSTGRES_PASSWORD=changeme
POSTGRES_DB=iron_wall_network_db
POSTGRES_PORT=5432

# PostgresExporter
POSTGRES_EXPORTER_VERSION=latest


POSTGRES_EXPORTER_PORT=9187

# Backend
BACKEND_DB_USER=changeme
BACKEND_DB_PASSWORD=changeme
BACKEND_DB_HOST=db
BACKEND_DB_NAME=iron_wall_network_db

# Backend-API
BACKEND_API_VERSION=9.2
BACKEND_API_PORT=8080

# Backend-event-streamer
EVENT_STREAMER_VERSION=latest

# MssageBroker
MESSAGE_BROKER_HOST=message-broker
MESSAGE_BROKER_VERSION=latest
MESSAGE_BROKER_PORT=6379
STREAM_GROUPS=account-events:account-event-group

# Nginx
NGINX_VERSION=1.23.3
NGINX_PORT=80
NGINX_SSL_PORT=443

# Grafana
GRAFANA_VERSION=latest
GRAFANA_PORT=3002

# Prometheus
PROMETHEUS_VERSION=latest
PROMETHEUS_PORT=9090
```

**Note:** Sensitive information like passwords should be masked or securely managed.

---

By following these steps, you can set up a secure and efficient environment for managing multiple services, with Nginx serving as a reverse proxy to manage and route traffic.