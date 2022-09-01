CREATE TABLE posts
  ("id" SERIAL PRIMARY KEY,
   "title" TEXT NOT NULL,
   "text" TEXT NOT NULL,
   "category_id" INT NOT NULL,
   "created_at" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "author_id" INT NOT NULL,
   "is_published" BOOLEAN NOT NULL);