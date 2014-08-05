import Data.List

sobelKernel = [[-1,0,1],[-2,0,2],[-1,0,1]]

createEvalList :: [Float] -> [Float] -> [(Float,Float)]
createEvalList list1 [] = []
createEvalList list1 list2 = [(x,y) | x <- list1] ++ createEvalList list1 (tail list2)
      	     	      where y = head list2

gaussian :: Float -> (Float,Float) -> Float
gaussian  sigma tuple = exp (-1* (((x**2)/(2*sigma**2)) + ((y**2)/(2*sigma**2))))
	 where x = fst tuple
	       y = snd tuple	

gaussianKernel :: Integer -> Float -> [[Float]]
gaussianKernel size sigma = to2DList dim dim (map (gaussian sigma) evalList)
	       where evalList = createEvalList [fromInteger(quot size (-2))..fromInteger(quot size 2)] (reverse [fromInteger(quot size (-2))..fromInteger(quot size 2)])
		     dim = fromInteger size

to2DList ::  Int -> Int -> [Float] -> [[Float]]
to2DList row col list 
	| row*col/=length list = error "dimension mismatch" 
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

main = do
	im <- readFile "sample_image.smi"
	let image = read im :: [[Float]]
	let image_string = list2string image
	writeFile "sample_image.smo" image_string
	print 5
