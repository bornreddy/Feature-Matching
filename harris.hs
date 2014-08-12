import Data.List
import FFT

sobelKernel = [[-1,0,1],[-2,0,2],[-1,0,1]]
bigSobel = [[4,3,2,1,0,-1,-2,-3,-4],[5,4,3,2,0,-2,-3,-4,-5],[6,5,4,3,0,-3,-4,-5,-6],[7,6,5,4,0,-4,-5,-6,-7],[8,7,6,5,0,-5,-6,-7,-8],[7,6,5,4,0,-4,-5,-6,-7], [6,5,4,3,0,-3,-4,-5,-6], [5,4,3,2,0,-2,-3,-4,-5], [4,3,2,1,0,-1,-2,-3,-4]]

createEvalList :: [Float] -> [Float] -> [(Float,Float)]
createEvalList list1 [] = []
createEvalList list1 list2 = [(x,y) | x <- list1] ++ createEvalList list1 (tail list2)
      	     	      where y = head list2

gaussian :: Float -> (Float,Float) -> Float
gaussian sigma tuple = exp (-1* (((x**2)/(2*sigma**2)) + ((y**2)/(2*sigma**2))))
	 where x = fst tuple
	       y = snd tuple	

gaussianKernel :: Integer -> Float -> [[Float]]
gaussianKernel size sigma = to2DList dim dim (map (gaussian sigma) evalList)
	       where evalList = createEvalList [fromInteger(quot size (-2))..fromInteger(quot size 2)] (reverse [fromInteger(quot size (-2))..fromInteger(quot size 2)])
		     dim = fromInteger size

to2DList ::  Int -> Int -> [Float] -> [[Float]]
to2DList row col list 
	| row * col /= length list = error "dimension mismatch!"
	| otherwise = helper col list
		where helper _ [] = []
		      helper c list = take c list : helper c (drop c list)

list2string :: [[Float]] -> [Char]
list2string list = foldr (\x y -> x (',' : y)) "\n" (map shows list)

pad :: Int -> Int -> [[Float]] -> [[Float]]
pad xdim ydim matrix = transpose $ padDim offsetx $ transpose $ padDim offsety matrix
    where offsetx = xdim - (length $ matrix!!0)
          offsety = ydim - length matrix

padDim :: Int -> [[Float]] -> [[Float]] 
padDim offset matrix 
    | offset >= 0 = [[0.0 | x<-[1..(length $ matrix!!0)]] | y<-[1..offset]]++matrix
    | otherwise = take (offset + length matrix) matrix

findPadPower :: Float -> Int
findPadPower dim = 2^(ceiling $ logBase 2 dim)

main = do
	im <- readFile "sample_image.smi"

	let image = read im :: [[Float]]
	let image_x = findPadPower $ fromIntegral $ length $ image!!0
	let image_y = findPadPower $ fromIntegral $ length $ image	
	let padded_image = pad image_x image_y image

	let gaussian_dim = fromIntegral $ ((min image_x image_y) - 1)
	--let gauss_ker = gaussianKernel gaussian_dim 1.5

	--let padded_gauss = reverse $ map reverse $ pad image_x image_y $ reverse $ map reverse $ pad (image_x `quot` 2) (image_y `quot` 2) gauss_ker
	--let padded_gauss = pad image_x image_y (transpose sobelKernel)
	let padded_gauss = pad image_x image_y sobelKernel
	--let padded_gauss = pad image_x image_y bigSobel
	


	--let gauss_ker = gaussianKernel 3 1.5
	--let padded_gauss = pad image_x image_y gauss_ker
	let fft_image = fft2 padded_image
	--for testing: so we can look at the fft_image
	let visual_fft_image = map decomplexify fft_image

	let fft_gauss = fft2 padded_gauss
	let convolution = zipWith (zipWith (*)) fft_image fft_gauss		
	let blurred_image = map decomplexify $ ifft2 convolution

	let image_string = list2string blurred_image
	writeFile "sample_image.smo" image_string
	print fft_image
