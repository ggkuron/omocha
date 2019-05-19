module ColladaSpec (spec) where

import Test.Hspec
import Omocha.Collada
import Control.Monad.IO.Class
import Omocha.Scene(Scene(..), Mesh(..), OmochaShaderType(..), DrawVertex(..), RenderInput(..))
import Graphics.GPipe (
    V2(..), V3(..), V4(..)
    )

spec :: Spec
spec = do
    describe "splitIn" $ do
        it "devide array into n stride groups" $ do
            splitIn 3 [0..10] `shouldBe` [[0,3,6,9], [1,4,7,10], [2,5,8]]
            splitIn 2 [0..10] `shouldBe` [[0,2,4,6,8,10], [1,3,5,7,9]]
            splitIn 1 [0..10] `shouldBe` [[0..10]]
    -- describe "readColladaFile" $ do
    --     it "read from a simple blender dae" $ do
    --         Just collada <- liftIO $ do
    --             readColladaFile "untitled.dae"
    --         (sceneFromCollada collada)
    --           `shouldBe` 
    --             Scene {
    --                 meshes = [
    --                   Mesh {
    --                     vertices = [
    --                         DrawVertex { dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0 },
    --                         DrawVertex { dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0 },
    --                         DrawVertex { dvPosition = V3 4.12902 3.745798 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 4.12902 8.200341e-2 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 4.12902 3.745798 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex { dvPosition = V3 7.792813 8.200341e-2 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 (-3.3), dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 (-0.5788838) 0.202383 0.7898954, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 (-0.5788838) 0.202383 0.7898954, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 (-0.5788838) 0.202383 0.7898954, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 0.8722414 0.4890757 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 0.8722414 0.4890757 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 0.8722414 0.4890757 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 (-3.3), dvNormal = V3 (-0.8570746) 0.5151923 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 (-0.8570746) 0.5151923 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 (-0.8570746) 0.5151923 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 4.171588, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 4.171588, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 4.171588, dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 (-4.171588), dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 0.0 0.5549718 (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 (-3.3), dvNormal = V3 0.0 0.5549718 (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 0.0 0.5549718 (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 0.0 (-0.5549718) (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 (-0.5549718) (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 0.0 (-0.5549718) (-0.8318692), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 (-4.171588), dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 (-4.53854e-7) (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 (-4.53854e-7) (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 (-4.53854e-7) (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 0.0 0.0 (-1.0), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 4.53854e-7 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 4.53854e-7 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 4.53854e-7 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.0 0.0 1.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 (-3.3), dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 (-3.3), dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.5814827 0.1798025 0.7934412, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 0.5814827 0.1798025 0.7934412, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.5814827 0.1798025 0.7934412, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 0.410701 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 0.0 0.410701 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 0.410701 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 0.4107009 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.0 0.4107009 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 0.4107009 0.9117701, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 0.8722417 0.4890752 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 (-3.3), dvNormal = V3 0.8722417 0.4890752 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 0.8722417 0.4890752 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 (-3.3), dvNormal = V3 (-0.8570749) 0.5151918 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 (-0.8570749) 0.5151918 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 (-0.8570749) 0.5151918 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 4.171588, dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 4.171588, dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 8.200341e-2 (-4.171588), dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 (-4.171588), dvNormal = V3 (-1.0) 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 0.0 0.5549719 (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 (-3.3), dvNormal = V3 0.0 0.5549719 (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 (-3.3), dvNormal = V3 0.0 0.5549719 (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 4.590883, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.12902 3.745798 4.171588, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 0.0 1.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 (-4.171588), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 (-4.590883), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 3.556886 3.745798 (-4.590883), dvNormal = V3 0.0 (-1.0) 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 8.364948 3.745798 4.590883, dvNormal = V3 0.0 (-0.5549719) (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.279996 5.680753 3.3, dvNormal = V3 0.0 (-0.5549719) (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 4.719998 5.680753 3.3, dvNormal = V3 0.0 (-0.5549719) (-0.8318691), dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 (-4.171588), dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 8.200341e-2 4.171588, dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0},
    --                         DrawVertex {dvPosition = V3 7.792813 3.745798 4.171588, dvNormal = V3 1.0 0.0 0.0, dvUv = V2 0.0 0.0}
    --                     ],
    --                     indices = Nothing,
    --                     offset = V3 0 0 0,
    --                     textureImage = Nothing,
    --                     shaderType = BoardShader
    --                   }
    --                 ],
    --                 camera = V3 0.0 0.0 0.0
    --                 }
