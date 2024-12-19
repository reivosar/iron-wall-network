ARG ALERT_NOTIFIER_VERSION
FROM python:${ALERT_NOTIFIER_VERSION}

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY /app/alert_handler.py /app/alert_handler.py

CMD ["python", "/app/alert_handler.py"]
