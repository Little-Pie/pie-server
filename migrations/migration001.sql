CREATE TABLE users
  ("id" SERIAL PRIMARY KEY,
   "name" TEXT NOT NULL,
   "login" TEXT NOT NULL,
   "password" TEXT NOT NULL,
   "data" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "admin" BOOLEAN NOT NULL);

ALTER TABLE users ADD COLUMN posting_news BOOLEAN NOT NULL;

--ALTER TABLE <table_name> DROP COLUMN <column_name>
--UPDATE <table_name> SET (login,password) = ('pie','123a') WHERE id = 1