import System.Environment (getArgs)
import Data.Word (Word8)
import Data.Array.Repa as R
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.DevIL (runIL, readImage, writeImage, IL, Image(RGB))
import Data.Array.Repa.Repr.ForeignPtr (F)

sobel = fromListUnboxed (Z :. 3 :. 3 :: DIM2) ([-1,0,1,-2,0,2,-1,0,1]::[Int])

gaussianKernel :: (Num a) => Int -> a -> Array U DIM2 Int 
gaussianKernel size sigma = fromListUnboxed (Z :. size :. size :: DIM2) ([1..size*size]::[Int])


getImage filePath = runIL $ do
    (RGB rgbData) <- readImage filePath
    return rgbData

main = do
	let im1 = "notredame.jpg"
	runIL $ do 
		(RGB rgbData) <- readImage im1
		writeImage ("output.jpg") (RGB rgbData) 
