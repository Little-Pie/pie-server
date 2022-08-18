CREATE TABLE users
  ("id" SERIAL PRIMARY KEY,
   "name" TEXT NOT NULL,
   "login" TEXT NOT NULL,
   "password" TEXT NOT NULL,
   "data" TIMESTAMP NOT NULL,
   "admin" BOOLEAN NOT NULL);

--ALTER TABLE <table_name> ADD COLUMN <column_name> <data_type>
--ALTER TABLE <table_name> DROP COLUMN <column_name>
--UPDATE <table_name> SET (login,password) = ('pie','123a') WHERE id = 1