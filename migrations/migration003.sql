CREATE TABLE categories
  ("id" SERIAL PRIMARY KEY,
   "name" TEXT NOT NULL,
   "parent_id" INT);