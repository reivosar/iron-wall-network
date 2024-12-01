以下の内容で `README.md` を作成します。このファイルは、プロジェクトの概要、設定手順、実行方法、および関連するコマンドを含むように設計されています。

---

### `README.md`

```markdown
# Iron Wall Network Backend API

## Overview

This is the backend API for the Iron Wall Network project, designed using Haskell and Servant. It follows clean architecture principles, providing modular and maintainable code.

## Features

- Account management (create, activate, suspend, close)
- Funds management (deposit, withdraw)
- Contact information management (update address, phone, emergency contact)
- Event-driven architecture with Redis and PostgreSQL
- Secure communication with HTTPS

## Prerequisites

To run the project, ensure the following dependencies are installed:

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- Docker and Docker Compose
- PostgreSQL
- Redis

## Setup

1. Clone the repository:

   ```bash
   git clone https://github.com/<your-repo>/iron-wall-network.git
   cd iron-wall-network
   ```

2. Create a `.env` file in the project root directory:

   ```env
   BACKEND_API_PORT=8080
   BACKEND_DB_USER=your_db_user
   BACKEND_DB_PASSWORD=your_db_password
   BACKEND_DB_HOST=your_db_host
   BACKEND_DB_NAME=your_db_name
   MESSAGE_BROKER_HOST=your_redis_host
   MESSAGE_BROKER_PORT=6379
   ```

3. Build the Docker images:

   ```bash
   docker-compose build
   ```

4. Start the services:

   ```bash
   docker-compose up
   ```

## Endpoints

The API exposes the following endpoints:

| Endpoint                             | HTTP Method | Description                       |
|--------------------------------------|-------------|-----------------------------------|
| `/create-account`                    | POST        | Create a new account             |
| `/approve-account`                   | POST        | Approve an account               |
| `/deposit-funds`                     | POST        | Deposit funds into an account    |
| `/withdraw-funds`                    | POST        | Withdraw funds from an account   |
| `/suspend-account`                   | POST        | Suspend an account               |
| `/activate-account`                  | POST        | Activate an account              |
| `/close-account`                     | POST        | Close an account                 |
| `/upsert-contact-info`               | POST        | Update user contact information  |
| `/upsert-phone-number`               | POST        | Update user phone number         |
| `/upsert-address`                    | POST        | Update user address              |
| `/upsert-emergency-contact`          | POST        | Update emergency contact         |

## Testing

To run unit tests, execute:

```bash
stack test
```

To manually test the API, use tools like [Postman](https://www.postman.com/) or `curl`.

## Deployment

To deploy the application, ensure all environment variables are set correctly and use:

```bash
docker-compose up --build
```

## License

This project is licensed under the MIT License. See the LICENSE file for more information.
```

---