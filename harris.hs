import System.Environment (getArgs)
import Data.Word (Word8)
import qualified Data.Array.Repa as R
import Data.Array.Repa hiding ((++),map)
import Data.Array.Repa.IO.DevIL (runIL, readImage, writeImage, IL, Image(Grey))
import Data.Array.Repa.Repr.ForeignPtr (F)
--import Data.Array.Repa.Algorithms

sobelKernel = fromListUnboxed (Z :. 3 :. 3 :: DIM2) ([-1,0,1,-2,0,2,-1,0,1]::[Float])

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

gaussianKernel :: Integer -> Float -> Array U DIM2 Float
gaussianKernel size sigma = fromListUnboxed (Z:.3:.3::DIM2) (map (gaussian sigma) evalList)
	       where evalList = createEvalList [fromInteger(quot size (-2))..fromInteger(quot size 2)] (reverse [fromInteger(quot size (-2))..fromInteger(quot size 2)])

--filterImage :: Array U DIM2 Float -> Array U DIM2 Float -> Array U DIM2 Float
--filterImage kernel image = conv.convolve kernel image     

getImage filePath = runIL $ do
    (Grey greyData) <- readImage filePath
    return greyData

     
--assume that all rows are of equal length

--convertToRepa :: [[Float]] -> Array F DIM2 Word8
--convertToRepa flatImageData = fromListUnboxed (Z :. w :. h :: DIM2) flatImageData
   -- where w = length flatImageData
   --       h = length flatImageData!!0

--makeImage flatImageData = runIL $ do
--  writeImage ("output.jpg") (Grey repaImage)
--  where repaImage = convertToRepa flatImageData

main = do
    let im1 = toList $ getImage "notredame.jpg"
    --in makeImage im1
    --let im1 = "notredame.jpg"
    --(Grey greyData) <- readImage im1
    --imageData <- toList greyData
    --(Grey filtered) <- filterImage sobelKernel greyData
    --writeImage ("output.jpg") (Grey greyData) 
    4 + 4
