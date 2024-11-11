CREATE TABLE bank_users (
    user_id SERIAL PRIMARY KEY,
    username VARCHAR(100) UNIQUE NOT NULL, 
    full_name VARCHAR(255) NOT NULL,  
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_user_contacts (
    user_id INT PRIMARY KEY REFERENCES bank_users(user_id) ON DELETE CASCADE,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_user_phone_numbers (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    phone_number VARCHAR(20) NOT NULL,
    type VARCHAR(50), 
    PRIMARY KEY (user_id, phone_number)
);

CREATE TABLE bank_user_addresses (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    address TEXT NOT NULL,
    type VARCHAR(50),
    PRIMARY KEY (user_id, address)
);

CREATE TABLE bank_user_emergency_contacts (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    contact_name VARCHAR(255) NOT NULL,
    contact_phone VARCHAR(20) NOT NULL,
    PRIMARY KEY (user_id, contact_name)
);

CREATE TABLE bank_user_auth (
    user_id INT PRIMARY KEY REFERENCES bank_users(user_id) ON DELETE CASCADE, 
    password_hash VARCHAR(255) NOT NULL, 
    failed_attempts INT DEFAULT 0, 
    account_locked BOOLEAN DEFAULT FALSE,
    last_failed_attempt TIMESTAMPTZ,
    password_reset_token VARCHAR(255),
    password_reset_token_expiry TIMESTAMPTZ,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE INDEX idx_bank_users_username ON bank_users(username);
CREATE INDEX idx_bank_user_contacts_email ON bank_user_contacts(email);
CREATE INDEX idx_bank_user_phone_numbers_user_id_phone_number ON bank_user_phone_numbers(user_id, phone_number);
CREATE INDEX idx_bank_user_addresses_user_id_address ON bank_user_addresses(user_id, address);
CREATE INDEX idx_bank_user_emergency_contacts_user_id_contact_name ON bank_user_emergency_contacts(user_id, contact_name);
CREATE INDEX idx_bank_user_auth_user_id ON bank_user_auth(user_id);
