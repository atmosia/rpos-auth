CREATE TABLE user_permissions(
    username VARCHAR(255) NOT NULL,
    module VARCHAR(255) NOT NULL,
    permission VARCHAR(255) NOT NULL,
    created TIMESTAMP NOT NULL DEFAULT NOW(),
    created_by VARCHAR(255) NOT NULL,

    PRIMARY KEY (username, permission)
);
