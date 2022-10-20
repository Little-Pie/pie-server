import Endpoints.CreateCategory (createCategoryTest)
import Endpoints.CreatePost (createPostTest)
import Endpoints.CreateUser (createUserTest)
import Endpoints.EditCategory (editCategoryTest)
import Endpoints.EditPost (editPostTest)
import Endpoints.GetImageById (getImageByIdTest)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  createUserTest
  createCategoryTest
  createPostTest
  editCategoryTest
  editPostTest
  getImageByIdTest
