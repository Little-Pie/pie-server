CREATE TABLE users
  ("id" SERIAL PRIMARY KEY,
   "name" TEXT NOT NULL,
   "login" TEXT NOT NULL,
   "password" TEXT NOT NULL,
   "created_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "is_admin" BOOLEAN NOT NULL,
   "is_author" BOOLEAN NOT NULL);

CREATE TABLE posts
  ("id" SERIAL PRIMARY KEY,
   "title" TEXT NOT NULL,
   "created_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "text" TEXT NOT NULL,
   "author_id" INT NOT NULL);

--ALTER TABLE <table_name> DROP COLUMN <column_name>
--UPDATE <table_name> SET (login,password) = ('pie','123a') WHERE id = 1