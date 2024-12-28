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

CREATE TABLE bank_user_email_contacts (
    email_contact_id SERIAL PRIMARY KEY,
    user_id INT PRIMARY KEY REFERENCES bank_users(user_id) ON DELETE CASCADE,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMPTZ DEFAULT NOW(),
    updated_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE bank_user_phone_number_contacts (
    phone_number_contact_id SERIAL PRIMARY KEY,
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    phone_number VARCHAR(20) NOT NULL,
    type VARCHAR(50) NULL,
    PRIMARY KEY (user_id, phone_number)
);

CREATE TABLE bank_user_addresses (
    address_id SERIAL PRIMARY KEY,
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    postal_code CHAR(8) NOT NULL,
    prefecture VARCHAR(50) NOT NULL,
    city VARCHAR(100) NOT NULL,
    town_area VARCHAR(200) NOT NULL,
    building_name VARCHAR(200),
    address_type VARCHAR(50),
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW() ON UPDATE NOW()
);

CREATE TABLE bank_user_emergency_contacts (
    user_id INT NOT NULL REFERENCES bank_users(user_id) ON DELETE CASCADE,
    contact_name VARCHAR(255) NOT NULL,
    contact_phone VARCHAR(20) NOT NULL,
    PRIMARY KEY (user_id, contact_name)
);

CREATE UNIQUE INDEX idx_bank_user_profiles_username ON bank_user_profiles(username);

CREATE UNIQUE INDEX idx_bank_user_email_contacts_email ON bank_user_email_contacts(email);
CREATE INDEX idx_bank_user_email_contacts_user_id ON bank_user_email_contacts(user_id);

CREATE INDEX idx_bank_user_phone_number_contacts_phone_number ON bank_user_phone_number_contacts(phone_number);
CREATE INDEX idx_bank_user_phone_number_contacts_user_id ON bank_user_phone_number_contacts(user_id);

CREATE INDEX idx_bank_user_addresses_user_id ON bank_user_addresses(user_id);
CREATE INDEX idx_bank_user_addresses_postal_code ON bank_user_addresses(postal_code);
CREATE INDEX idx_bank_user_addresses_address_type ON bank_user_addresses(address_type);

CREATE INDEX idx_bank_user_emergency_contacts_user_id ON bank_user_emergency_contacts(user_id);
CREATE INDEX idx_bank_user_emergency_contacts_contact_phone ON bank_user_emergency_contacts(contact_phone);
