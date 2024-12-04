CREATE EXTENSION IF NOT EXISTS pgcrypto;

INSERT INTO system_users (user_name, password_hash, auth_key_hash)
VALUES (
    'admin',
    encode(hmac('password', '${PASSWORD_SECRET_KEY}', 'sha512'), 'base64'),
    encode(hmac('admin_auth_key', '${AUTH_KEY_SECRET_KEY}', 'sha512'), 'base64')
);