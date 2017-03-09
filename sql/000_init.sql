CREATE TABLE user_permissions(
    username VARCHAR(255) NOT NULL,
    module VARCHAR(255) NOT NULL,
    permission VARCHAR(255) NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT 'f',
    created_by VARCHAR(255) NOT NULL,
    deleted_by VARCHAR(255),
    created_on TIMESTAMP DEFAULT NOW() NOT NULL,
    deleted_on TIMESTAMP,

    PRIMARY KEY (username, module, permission, created_on),
    CHECK (created_on < deleted_on)
);

CREATE TABLE apikey (
    key VARCHAR(255) NOT NULL UNIQUE,
    deleted BOOLEAN NOT NULL DEFAULT 'f',
    created_by VARCHAR(255) NOT NULL,
    deleted_by VARCHAR(255),
    created_on TIMESTAMP DEFAULT NOW() NOT NULL,
    deleted_on TIMESTAMP,

    PRIMARY KEY (key, created_on),
    CHECK (created_on < deleted_on)
);
CREATE INDEX apikey_key ON apikey(key);

CREATE TABLE apikey_permission(
    key VARCHAR(255) NOT NULL,
    module VARCHAR(255) NOT NULL,
    permission VARCHAR(255) NOT NULL,
    deleted BOOLEAN NOT NULL DEFAULT 'f',
    created_by VARCHAR(255) NOT NULL,
    deleted_by VARCHAR(255),
    created_on TIMESTAMP DEFAULT NOW() NOT NULL,
    deleted_on TIMESTAMP,

    PRIMARY KEY (key, module, permission, created_on),
    FOREIGN KEY (key) REFERENCES apikey(key),
    CHECK (created_on < deleted_on)
);
CREATE INDEX apikey_permission_key ON apikey_permission(key);
CREATE INDEX apikey_permission_kmp ON
    apikey_permission(key, module, permission);
