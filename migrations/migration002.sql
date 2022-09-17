CREATE TABLE posts
  ("id" SERIAL PRIMARY KEY,
   "title" TEXT NOT NULL,
   "text" TEXT NOT NULL,
   "categoryId" INT NOT NULL,
   "createdAt" TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP NOT NULL,
   "authorId" INT NOT NULL,
   "isPublished" BOOLEAN NOT NULL);