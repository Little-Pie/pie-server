CREATE TABLE images
  ("id" SERIAL PRIMARY KEY,
   "postId" INT NOT NULL,
   "base64Image" TEXT NOT NULL,
   "contentType" TEXT NOT NULL);