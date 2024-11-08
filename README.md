Here is the README in Markdown format based on the provided project information:

---

# Iron Wall Network

Iron Wall Network is an integrated infrastructure designed to manage a Haskell-based backend API, Grafana for monitoring, and Prometheus for metrics collection. This system uses Nginx as a reverse proxy with ModSecurity for enhanced security measures.

## Table of Contents
- [Features](#features)
- [Accessing Services](#accessing-services)
- [Security Considerations](#security-considerations)
- [Installation](#installation)
- [Environment Variables](#environment-variables)

## Features

1. **Backend API**  
   - Built in Haskell and accessible via the `/api/` endpoint.
   - Uses the Servant library for API definition and implementation.

2. **Grafana**  
   - Provides visualization and monitoring at `/grafana/`.

3. **Prometheus**  
   - Handles metrics collection and alerts, accessible at `/prometheus/`.

4. **Nginx Reverse Proxy**  
   - Routes traffic to appropriate services based on URL paths.
   - Integrates ModSecurity for enhanced request filtering and monitoring.
   - Configured with rate limiting to prevent misuse.

## Accessing Services
After starting the services, access them at the following endpoints:

- **API Endpoint:** [https://localhost/api/](https://localhost/api/)
- **Grafana Dashboard:** [https://localhost/grafana/](https://localhost/grafana/)
- **Prometheus Interface:** [https://localhost/prometheus/](https://localhost/prometheus/)

## Security Considerations

- **ModSecurity**  
   - Acts as a web application firewall for HTTP traffic filtering and monitoring.
  
- **Rate Limiting**  
   - Controls incoming request rate to mitigate potential denial-of-service attacks.
  
- **SSL/TLS**  
   - Enables encrypted communication between clients and server.

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
BACKEND_VERSION=9.2                      # Version of backend service (Not sensitive)
BACKEND_PORT=8080                        # Backend service port (Not sensitive)
BACKEND_DB_USER=                         # Database user for backend (Sensitive)
BACKEND_DB_PASSWORD=                     # Password for backend database user (Sensitive)
BACKEND_DB_HOST=db                       # Database host (Not sensitive)
BACKEND_DB_NAME=iron_wall_network_db     # Database name for backend (Not sensitive)

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
