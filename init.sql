CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS users (
    id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    login VARCHAR(100) NOT NULL,
    password VARCHAR(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS houses (
    id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    title VARCHAR(100) NOT NULL,
    address TEXT NOT NULL,
    user_id UUID NOT NULL references users(id) on delete cascade
);

CREATE TABLE IF NOT EXISTS device_types (
    id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    title VARCHAR(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS devices (
    id UUID PRIMARY KEY NOT NULL DEFAULT uuid_generate_v4(),
    title VARCHAR(100) NOT NULL,
    type_id UUID NOT NULL references device_types(id) on delete cascade,
    house_id UUID NOT NULL references houses(id) on delete cascade,
    serial_number VARCHAR(100) NOT NULL,
    status VARCHAR(50) NULL,
    last_updated TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS telemetry (
    device_id UUID NOT NULL references devices(id) on delete cascade,
    data jsonb NOT NULL DEFAULT '{}'::jsonb,
    changed_at TIMESTAMP NOT NULL DEFAULT now()
);

GRANT ALL PRIVILEGES ON DATABASE postgres TO postgres;


-- generate test data
INSERT INTO users (id, login, "password") VALUES ('d48c61c3-1739-4e73-8a07-8c58dde1808f', 'admin', 'password');
INSERT INTO houses (id, title, address, user_id) VALUES ('c4ffe622-75dd-489e-86e5-b268766e29d0', 'Admin House', '', 'd48c61c3-1739-4e73-8a07-8c58dde1808f');
INSERT INTO device_types (id, title) VALUES ('06a74d3a-1db6-42c5-b417-9c9dd0dfa208', 'Type 1');
INSERT INTO devices (id, title, type_id, house_id, serial_number, "status") VALUES ('e06a2794-62d4-44f8-afbd-0e54120c087b', 'Test device', '06a74d3a-1db6-42c5-b417-9c9dd0dfa208', 'c4ffe622-75dd-489e-86e5-b268766e29d0', '4CE0460D0G', 'off');
