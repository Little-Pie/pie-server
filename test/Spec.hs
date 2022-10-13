import Endpoints.CreateUser (createUserTest)
import Endpoints.CreateCategory (createCategoryTest)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  createUserTest
  createCategoryTest
