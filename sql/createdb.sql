BEGIN TRANSACTION;

CREATE TABLE IF NOT EXISTS users (
  id SERIAL PRIMARY KEY,
  email TEXT NOT NULL,
  salt TEXT NOT NULL,
  hashedPassword TEXT NOT NULL
);

-- avoid empty table (selda workaround for max on empty table)
INSERT INTO users VALUES (0, '', '', '')
ON CONFLICT DO NOTHING;

SET TIME ZONE 'UTC';

COMMIT TRANSACTION;
