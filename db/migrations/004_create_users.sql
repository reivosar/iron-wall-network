CREATE TABLE bank_users (
    user_id SERIAL PRIMARY KEY,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_user_profiles (
    user_id INT PRIMARY KEY REFERENCES bank_users(user_id) ON DELETE CASCADE,
    username VARCHAR(100) NOT NULL, 
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
    type VARCHAR(50) NULL,
    PRIMARY KEY (user_id, phone_number)
);

CREATE TABLE bank_user_addresses (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    address TEXT NOT NULL,
    type VARCHAR(50) NULL,
    PRIMARY KEY (user_id, address)
);

CREATE TABLE bank_user_emergency_contacts (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    contact_name VARCHAR(255) NOT NULL,
    contact_phone VARCHAR(20) NOT NULL,
    PRIMARY KEY (user_id, contact_name)
);

CREATE INDEX idx_bank_user_profiles_username ON bank_user_profiles(username);
CREATE INDEX idx_bank_user_contacts_email ON bank_user_contacts(email);
CREATE INDEX idx_bank_user_phone_numbers_user_id_phone_number ON bank_user_phone_numbers(user_id, phone_number);
CREATE INDEX idx_bank_user_addresses_user_id_address ON bank_user_addresses(user_id, address);
CREATE INDEX idx_bank_user_emergency_contacts_user_id_contact_name ON bank_user_emergency_contacts(user_id, contact_name);
CREATE INDEX idx_bank_user_profiles_user_id_updated_at ON bank_user_profiles(user_id, updated_at);