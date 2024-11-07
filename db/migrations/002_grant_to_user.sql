GRANT CONNECT ON DATABASE iron_wall_network_db TO iron_wall_network_user;

GRANT USAGE ON SCHEMA public TO iron_wall_network_user;
GRANT CREATE ON SCHEMA public TO iron_wall_network_user;

GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO iron_wall_network_user;

ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO iron_wall_network_user;

GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO iron_wall_network_user;
ALTER DEFAULT PRIVILEGES IN SCHEMA public
    GRANT USAGE, SELECT ON SEQUENCES TO iron_wall_network_user;