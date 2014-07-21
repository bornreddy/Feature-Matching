import System.Environment (getArgs)
import Data.Word (Word8)
--import Data.Array.Repa as R
--import Data.Array.Repa hiding ((++))import Data.Array.Repa.IO.DevIL (runIL, readImage, writeImage, IL, Image(RGB))
--import Data.Array.Repa.Repr.ForeignPtr (F)

--sobel = fromListUnboxed (Z :. 3 :. 3 :: DIM2) ([-1,0,1,-2,0,2,-1,0,1]::[Int])

createEvalList :: [Float] -> [Float] -> [(Float,Float)]
createEvalList list1 [] = []
createEvalList list1 list2 = [(x,y) | x <- list1] ++ createEvalList list1 (tail list2)
	       	     	      where y = head list2

--gaussianKernel :: (Num a) => Int -> a -> Array U DIM2 Int 
--gaussianKernel size sigma = fromListUnboxed (Z :. size :. size :: DIM2) ([1..size*size]::[Int])

gaussian :: Float -> (Float,Float) -> Float
gaussian  sigma tuple = exp (-1* (((x**2)/(2*sigma**2)) + ((y**2)/(2*sigma**2))))
	 where x = fst tuple
	       y = snd tuple
	

gaussianKernel :: Integer -> Float -> [Float]
gaussianKernel size sigma = map (gaussian sigma) evalList
	       where evalList = createEvalList [fromInteger(quot size (-2))..fromInteger(quot size 2)] (reverse [fromInteger(quot size (-2))..fromInteger(quot size 2)])


     


--getImage filePath = runIL $ do
--    (RGB rgbData) <- readImage filePath
--    return rgbData

--main = do
--	let im1 = "notredame.jpg"
--	runIL $ do 
--		(RGB rgbData) <- readImage im1
--		writeImage ("output.jpg") (RGB rgbData) 
