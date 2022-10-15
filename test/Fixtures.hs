module Fixtures where

import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.Entities.Post (Post (..))

userAdminAuthor :: User
userAdminAuthor = User 1 "name" "login" "password" undefined True True

userAdminNotAuthor :: User
userAdminNotAuthor = User 2 "name" "login" "password" undefined True False

userNotAdminAuthor :: User
userNotAdminAuthor = User 3 "name" "login" "password" undefined False True

post :: Post
post = Post 1 "title" "text" 1 undefined 1 True

categoryRoot :: Category
categoryRoot = Category 1 "name" Nothing

category :: Category
category = Category 2 "name" (Just 1)