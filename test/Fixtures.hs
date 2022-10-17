module Fixtures where

import Types.Entities.User (User (..))
import Types.Entities.Category (Category (..))
import Types.Entities.Post (Post (..))
import Types.Entities.Image (Image(..))

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

image :: Image
image = Image 1 1 "iVBORw0KGgoAAAANSUhEUgAAACEAAAAgCAIAAAAT2oadAAAAKklEQVRIie3NMQ0AAAgDsKnCAP7dIABM8DXp30zXtzgcDofD4XA4HA7HWWvp4ewLBXacAAAAFHRFWHRTb2Z0d2FyZQBZYW5kZXguRGlza05f+JEAAAAASUVORK5CYII=" "png"