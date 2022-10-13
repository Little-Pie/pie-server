import Endpoints.CreateUser (createUserTest)
import Endpoints.CreateCategory (createCategoryTest)
import Endpoints.CreatePost (createPostTest)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  createUserTest
  createCategoryTest
  createPostTest
