# server

## Description:
* This is news web server with REST API, it accepts HTTP requests and returns responses in JSON format.

## Installation:
* Clone repository
* Rename "config-template.json" to "config.json"
* Add data for connection to PostreSQL database in "config.json"
* You can change loggingLevel in "config.json" to Debug, Release, Warning or Error

## Structure:
Module Main reads config, parses it and case of success connects to PostgreSQL database and starts app server, which is based on Warp.

Besides Main contains functions to run migrations, to open and to close TXT-file for logging. Filling tables received from migrations with fixtures for testind server can be initialized by command line argument "f".

Main runs an application which is imported from module Routing. This application contains all possible endpoints that can be called by user. There modules for handling business logic of every endpoint.

The ReaderT Design Pattern and Handle Pattern were used in the development of the code.

Handling of JSON-files and config is carried out using libraries "aeson" and "aeson-pretty".

The business logic of the project is covered by unit-tests, which are carried out using the library "hspec".

Libraries are used to work with the PostgreSQL database: "postgresql-simple" and "postgresql-simple-migration".

The server also uses hashing to securely store user passwords. Library "cryptonite" is used for that.

## Server API (endpoints):

*All request bodies should be in JSON format*

### User

* /createUser - POST-request for creation user (only available for admins)

Request body should contain: "name", "login", "password", "isAdmin", "isAuthor"

* /users - GET-request for getting a list of users

### Posts

* /createPost - POST-request for creation post (only available for authors)

Request body should contain: "title", "text", "categoryId", "isPublished", "base64Images", "contentTypes"

*Post can be created without images, use empty list in "base64Images" for that*

* /editPost - POST-request for editing post (only available for author of post)

Request body should contain "postId" and may contain "title", "text", "categoryId", "isPublished", "base64Images", "contentTypes"

* /posts - GET-request for getting a list of posts

This endpoint supports **filtration**, **sorting** and **searching**

***Filters***
1. createdAt
2. createdUntil
3. createdSince
4. author
5. categoryId
6. title (by occurrence of a substring)
7. text (by occurrence of a substring)

*example:* "/posts?author=John

***Sorting***
1. by category name
2. by author name
3. by title
4. by number of images

*example:* "/posts?sortBy=title

***Search***
Searches for a given string that can be found either in the text content, or in the author's name, or in the category name.

*example:* "/posts?search=hello

### Categories

* /createCategory - POST-request for creation category (only available for admins)

Request body should contain "name" and may contain "parentCategoryId"

* /editCategory - POST-request for editing category (only available for admins)

Request body should contain "categoryId" and may contain "name" and "parentCategoryId"

* /categories - GET-request for getting a list of categories

### Images

* /getImageById - GET-request for getting an image by id

*example:* /posts?id=1

