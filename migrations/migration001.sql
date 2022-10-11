CREATE TABLE users
  ("id" SERIAL PRIMARY KEY,
   "name" TEXT NOT NULL,
   "login" TEXT NOT NULL UNIQUE,
   "password" TEXT NOT NULL,
   "createdAt" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "isAdmin" BOOLEAN NOT NULL,
   "isAuthor" BOOLEAN NOT NULL);

--ALTER TABLE <table_name> DROP COLUMN <column_name>
--UPDATE <table_name> SET (login,password) = ('pie','123a') WHERE id = 1
--select * from users limit 10 offset 3;
--SELECT * FROM posts JOIN images ON images."postId" = posts.id WHERE posts.id = 1; 