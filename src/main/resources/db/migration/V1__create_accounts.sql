CREATE TABLE accounts
(
  id          SERIAL PRIMARY KEY,
  name        TEXT NOT NULL,
  description TEXT
);

CREATE TABLE entries
(
  id          SERIAL PRIMARY KEY,
  description TEXT,
  amount      DECIMAL                  NOT NULL,
  date        TIMESTAMP WITH TIME ZONE NOT NULL,
  account_id  INTEGER REFERENCES accounts (id)
);

CREATE TABLE attachments
(
  id       SERIAL PRIMARY KEY,
  url      TEXT NOT NULL,
  entry_id INTEGER REFERENCES entries (id)
);

