# Iron Wall Network

Iron Wall Network is an integrated infrastructure combining multiple components for secure, efficient backend API management, event-driven processing, monitoring, and visualization. The system is built using a Haskell backend API, Grafana for monitoring, Prometheus for metrics collection, and Nginx as a reverse proxy. It includes features for handling events, processing data, ensuring system security, and performing container security scans.

## Table of Contents
- [Features](#features)
- [Accessing Services](#accessing-services)
- [Security Considerations](#security-considerations)
- [Event Processing Flow](#event-processing-flow)
- [Container Security Scans with Trivy](#container-security-scans-with-trivy)
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

### 5. **Trivy Scanning**  
   - **Purpose**: Scans Docker images for vulnerabilities to improve container security.
   - **Integration**: Easily integrates into the CI/CD pipeline for ongoing security assessments.

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

For details, refer to the [Event Flow Example](#event-flow-example) section.

## Container Security Scans with Trivy

### Trivy Overview
The `trivy-scan` service integrates the [Trivy](https://aquasecurity.github.io/trivy/) vulnerability scanner to perform container security checks. It scans Docker images for vulnerabilities in:
- Operating systems
- Libraries
- Application dependencies

### Scan Workflow
A shell script (`trivy-scan.sh`) automates scanning all services defined in the Docker Compose file. It generates vulnerability reports for each service.

### Usage
Run the following command to perform a Trivy scan:
```bash
docker-compose run --rm trivy-scan
```

This will scan all container images and output the results to a timestamped directory under the `reports` folder. Reports are saved in JSON format.

### Example Command to Scan Images
If you wish to scan individual images manually:
```bash
trivy image --severity CRITICAL,HIGH <image-name>
```

### Sample Output
The following is an example of a Trivy JSON report:
```json
{
  "Target": "iron-wall-network-backend-api:latest",
  "Vulnerabilities": [
    {
      "VulnerabilityID": "CVE-2023-12345",
      "Severity": "HIGH",
      "Description": "Description of the vulnerability",
      "InstalledVersion": "1.2.3",
      "FixedVersion": "1.2.4"
    },
    {
      "VulnerabilityID": "CVE-2023-67890",
      "Severity": "CRITICAL",
      "Description": "Another vulnerability description",
      "InstalledVersion": "4.5.6",
      "FixedVersion": "4.5.7"
    }
  ]
}
```

### Notes
- Ensure that Trivy is installed on your system if running outside the Docker Compose setup.
- Use the `--exit-code` flag to fail the build in CI/CD pipelines if vulnerabilities exceed acceptable thresholds.

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

Refer to the example `.env` file provided in the repository for setting up the environment variables.
