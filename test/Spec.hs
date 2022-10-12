import Endpoints.CreateUser (createUserTest)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  createUserTest
