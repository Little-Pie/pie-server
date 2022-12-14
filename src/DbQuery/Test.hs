module DbQuery.Test where

import Config (App)
import Control.Monad (void)
import Database.PostgreSQL.Simple (execute, executeMany, execute_)
import Helpers (withDbConnection)

dropTables :: App ()
dropTables = do
  void $ withDbConnection (`execute_` "drop table if exists posts")
  void $ withDbConnection (`execute_` "drop table if exists users")
  void $ withDbConnection (`execute_` "drop table if exists categories")
  void $ withDbConnection (`execute_` "drop table if exists images")
  void $ withDbConnection (`execute_` "drop table if exists schema_migrations")

fillTables :: App ()
fillTables = do
  void $
    withDbConnection
      ( \conn ->
          executeMany
            conn
            "INSERT INTO users (name,login,password,\"isAdmin\",\"isAuthor\") VALUES (?,?,?,?,?)"
            [ ( "Admin" :: String,
                "Admin" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                True,
                True
              ),
              ( "Alena" :: String,
                "Alena" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                False,
                True
              ),
              ( "Oleg" :: String,
                "Oleg" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                True,
                False
              ),
              ( "Polina" :: String,
                "Polina" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                False,
                True
              ),
              ( "Katya" :: String,
                "Katya" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                False,
                True
              ),
              ( "Gena" :: String,
                "Gena" :: String,
                "a86807bb96a714fe9b22425893e698334cd71e36b0eef2be" :: String,
                False,
                False
              )
            ]
      )
  void $
    withDbConnection
      ( \conn ->
          execute
            conn
            "INSERT INTO categories (name) VALUES (?)"
            ["General" :: String]
      )
  void $
    withDbConnection
      ( \conn ->
          executeMany
            conn
            "INSERT INTO categories (name,\"parentId\") VALUES (?,?)"
            [ ("Green" :: String, 1 :: Int),
              ("Red" :: String, 1 :: Int),
              ("Tree" :: String, 2 :: Int),
              ("Tomato" :: String, 3 :: Int)
            ]
      )
  void $
    withDbConnection
      ( \conn ->
          executeMany
            conn
            "INSERT INTO posts (title,text,\"categoryId\",\"authorId\",\"isPublished\") \
            \VALUES (?,?,?,?,?)"
            [ ( "GreenPeace" :: String,
                "Save Nature" :: String,
                2 :: Int,
                2 :: Int,
                True
              ),
              ( "Apples" :: String,
                "Eat apples, stay healthy" :: String,
                4 :: Int,
                1 :: Int,
                True
              ),
              ( "Tomatoes" :: String,
                "Tomatoes are red" :: String,
                5 :: Int,
                1 :: Int,
                False
              ),
              ( "Cucumbers" :: String,
                "Cucumbers are green" :: String,
                2 :: Int,
                2 :: Int,
                True
              ),
              ( "Blood" :: String,
                "Blood is red" :: String,
                3 :: Int,
                4 :: Int,
                True
              ),
              ( "Leaves" :: String,
                "Leaves are on the tree" :: String,
                4 :: Int,
                4 :: Int,
                True
              ),
              ( "Traffic light" :: String,
                "You should pass across the street only when it's green light" :: String,
                2 :: Int,
                5 :: Int,
                True
              ),
              ( "Pineapples" :: String,
                "Pineapples are not red" :: String,
                3 :: Int,
                5 :: Int,
                True
              ),
              ( "Grass" :: String,
                "Grass is green" :: String,
                2 :: Int,
                1 :: Int,
                True
              )
            ]
      )
  void $
    withDbConnection
      ( \conn ->
          executeMany
            conn
            "INSERT INTO images (\"postId\",\"base64Image\",\"contentType\") VALUES (?,?,?)"
            [ (3 :: Int, "iVBORw0KGgoAAAANSUhEUgAAACwAAAAbCAYAAAAH+20UAAAAAXNSR0IArs4c6QAAAARnQU1BAACxjwv8YQUAAAAJcEhZcwAAEnQAABJ0Ad5mH3gAAAAUdEVYdFNvZnR3YXJlAFlhbmRleC5EaXNrTl/4kQAAA2NJREFUWEedWAty5SAMC6Q96R5yT7U3aF9WslEwDiSv1Ywagj8YxeFlWv7++3Ns22uboVSYgLoVu5aL37GVUrbmZmMfjH6l2Tte3Reo7bodPqqn7VrX6UswSVz0OC4rDYiLDmgLE8pZWtW8xrhuxxzWlEgrEZG5G47XF/58m5LMT2XLgTHvQeaKLK/Dr4g1VZCKYyN9MGJ8ZSy4Y1JjJ/2wAWyQ/uT2+uafgVZDI9bpO7RdNvgYTmHuCVdfiZGvT3C/XBOBrbl6xubgj892Y2oRYyDtUKvZZSuQrOdAb2POm6OPHH7PJ0BGxHWkes55gYyrcZyb417JdXyPW+WvJmaDvxAgxz5lUM8K0R4T75gls79QYSWlXIcX6j3vttHu4FpThX8DJuOpopOF95krZPtqTLSXbq0sT4IDbyeJkoyC1NTVlQqPlfGBGerhlV3F7riS+U14RN7tiHXfuih3sR3yW/lXO19hJNWDohaSneB9tp/Fsh3aOc6rWuQ42C5UyHtYzIgq8nzOivoTeQO9sHt4a/WNZK6Q7Xf31sM8U3mpFXto9zYH4hGcAf3t55lNu85w+JDm14tWj4oZFb9wO9VGzvGJeS7Fsb8VbwVHp3gfEeey3WN6oRH5PkK2p5joVyt75VSOf6ic7cQcyNyztCsl70/gRCH1u0/EDygq6tR6YzFUkvQcNm2Ix+W0h4ciGuLCwtXvWiQRFxRybLRnm22m8SzYAkC5ymEEC/KirraG1hoRuUejPRcq28zXnoyNQqEZdJoplHEukB5aXNT7XHl847zSalSO5k/onhsmeAj0HsW3aOU3J3ciYl78qJ/GfYc3knSbeo7FYqbQvm8f+HyzvK1QnSr8gBF3hOyId/b3RVQdVi5Ew0s33xnBYiKk9J3aOcccri6L5PpPUEYTKS7Ak8HInTU3bST6QZjhLc5gEVRH4Mjv/B2Qak/ocQ4ruI2HgohLkRg/KZxz3GG23jt4/OEY7KjTGOyEfKRctK98BYvjHMbnE9ZcJvN72DUxkecUGMczH+Envu+CXw/Wr9OebL84Udm4sBQVBVqlWixp5hftGcPaJHApph+bd20/EglV0xGxuhZ/4RuBTor9IUjruyJwa/X8IIJUnW+En6UO2OM3zxOCZPPRnsMenHCmcfse2/QeIUVb9vlybvAAAAABJRU5ErkJggg==" :: String, "png" :: String),
              (1 :: Int, "iVBORw0KGgoAAAANSUhEUgAAAEgAAABJCAIAAAAR0/e1AAAAeUlEQVRoge3PQQ3AMADEsHIby6Edhr2iqywFQHye91xZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwAGBjZZfwD2rw+fh7pooO2gCAAAABR0RVh0U29mdHdhcmUAWWFuZGV4LkRpc2tOX/iRAAAAAElFTkSuQmCC" :: String, "png" :: String)
            ]
      )
