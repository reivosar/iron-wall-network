# Backend API

## Overview
This repository contains the backend API for managing bank accounts, including functionalities such as creating accounts, approving accounts, handling deposits and withdrawals, and updating user information.

## Features
- **Account Management**: Create, approve, suspend, and close accounts.
- **Funds Handling**: Deposit and withdraw funds securely.
- **Contact Information**: Upsert user and emergency contact information.
- **Address Management**: Add or update user addresses.

## Requirements
- **Haskell**: Version 9.6.6
- **Stack**: Latest version recommended
- **Dependencies**:
  - PostgreSQL
  - Redis
  - `libpq-dev`, `zlib1g-dev` (for building)
  
## Getting Started

### 1. Clone the Repository
```bash
git clone https://github.com/your-org/iron-wall-network.git
cd iron-wall-network/backend-api
```

### 2. Build the Application
Use `stack` to set up the Haskell environment and build dependencies:
```bash
stack setup
stack build
```

### 3. Run the Application
To start the server:
```bash
stack exec backend-exe
```

### 4. API Endpoints
The API is defined using Servant. Below are some key endpoints:

| HTTP Method | Endpoint                  | Description               |
|-------------|---------------------------|---------------------------|
| `POST`      | `/accounts/create`        | Create a new account      |
| `POST`      | `/accounts/approve`       | Approve an account        |
| `POST`      | `/accounts/deposit`       | Deposit funds             |
| `POST`      | `/accounts/withdraw`      | Withdraw funds            |
| `POST`      | `/accounts/suspend`       | Suspend an account        |
| `POST`      | `/accounts/activate`      | Activate an account       |
| `POST`      | `/accounts/close`         | Close an account          |
| `POST`      | `/accounts/contact/upsert`| Upsert user contact info  |
| `POST`      | `/accounts/address/upsert`| Upsert user address       |

### 5. Environment Variables
The application requires the following environment variables to be configured:

| Variable Name       | Description                |
|---------------------|----------------------------|
| `BACKEND_API_PORT`  | Port for the backend API   |
| `BACKEND_DB_HOST`   | PostgreSQL host            |
| `BACKEND_DB_USER`   | PostgreSQL user            |
| `BACKEND_DB_PASSWORD` | PostgreSQL password       |
| `BACKEND_DB_NAME`   | PostgreSQL database name   |
| `MESSAGE_BROKER_HOST` | Redis host               |
| `MESSAGE_BROKER_PORT` | Redis port               |

## Running with Docker

### Build and Run the Container
```bash
docker-compose up --build
```

### Docker Compose Configuration
Ensure the `docker-compose.yml` contains the following services:
- `backend-api`: Runs the Haskell backend.
- `db`: PostgreSQL database.
- `message-broker`: Redis message broker.

## Contributing
Contributions are welcome! Please fork the repository and submit a pull request.

## License
This project is licensed under the MIT License.
