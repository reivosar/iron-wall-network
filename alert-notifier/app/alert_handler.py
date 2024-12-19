from flask import Flask, request
import json
import os
from datetime import datetime

app = Flask(__name__)
LOG_DIR = "/var/log/alert-notifier"

@app.route('/alert', methods=['POST'])
def handle_alert():
    if not os.path.exists(LOG_DIR):
        os.makedirs(LOG_DIR)

    data = request.json
    timestamp = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    file_path = os.path.join(LOG_DIR, f"alert_{timestamp}.json")

    with open(file_path, 'w') as f:
        json.dump(data, f, indent=4)

    return "Alert received and logged.", 200

if __name__ == '__main__':
    port = int(os.getenv("ALERT_NOTIFIER_PORT", 9096)) 
    app.run(host='0.0.0.0', port=port)
