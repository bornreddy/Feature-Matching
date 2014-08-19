import Data.List
import FFT
import Data.Complex

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

centeredPad :: Int -> Int -> [[Float]] -> [[Float]]
centeredPad xdim ydim image = transpose $ padDimCentered (xdim-(length $ image!!0)) $ transpose $ padDimCentered (ydim-(length image)) image

padDimCentered :: Int -> [[Float]] ->[[Float]]
padDimCentered dim list 
                | dim `mod` 2 == 0 = padding ++ list ++padding
                | otherwise = padding ++ list ++ padding ++ [[0.0 | x<-[1..(length $ list!!0)]]]
               where padding = [[0.0 | x<- [1..(length $ list!!0)]] | y<-[1..(quot dim 2)]]

padDim :: Int -> [[Float]] -> [[Float]] 
padDim offset matrix 
    | offset >= 0 = [[0.0 | x<-[1..(length $ matrix!!0)]] | y<-[1..offset]]++matrix
    | otherwise = take (offset + length matrix) matrix

findPadPower :: Float -> Int
findPadPower dim = 2^(ceiling $ logBase 2 dim)

--trim_centered :: Int -> [[Float]] -> [[Float]]
--trim_centered dim padded_image = drop ((length padded_image)-dim) padded_image

--trim_dim :: Int [[Float]] -> [[Float]]


pointwise_mult :: [[Complex Float]] -> [[Complex Float]] -> [[Complex Float]]
pointwise_mult a b = zipWith (zipWith (*)) a b

rearrange_quadrants :: [[Float]] -> [[Float]]
rearrange_quadrants matrix = transpose $ rearrange_dim $ transpose $ rearrange_dim matrix

rearrange_dim :: [[Float]] -> [[Float]]
rearrange_dim matrix  = (drop (quot (length matrix) 2) matrix) ++ (take (quot (length matrix) 2) matrix)

main = do
    im <- readFile "sample_image.smi"

    let image = read im :: [[Float]]
        gauss_dim = 3
        pad_x = findPadPower $ fromIntegral (length $ image!!0)
        pad_y = findPadPower $ fromIntegral (length image)
        padded_image = centeredPad pad_x pad_y image
        padded_gauss = centeredPad pad_x pad_y (gaussianKernel (fromIntegral 15) 16)
        --padded_gauss = gaussianKernel (fromIntegral 9) 9
        fft_image = fft2 padded_image
        fft_gauss = fft2 padded_gauss
        convolution = pointwise_mult fft_image fft_gauss     
       -- identity_conv = pointwise_mult fft_image (fft2 [[1 | x<-[1..pad_x]] | x<-[1..pad_x]])
        --identity_orig = map decomplexify $ ifft2 identity_conv
        blurred_image = rearrange_quadrants $ map decomplexify $ ifft2 convolution
        
       -- trimmed_image = transpose $ trim (length $ image!!0) $ transpose $ trim (length image) blurred_image
        image_string = list2string blurred_image

    writeFile "sample_image.smo" image_string
    print pad_x
    print pad_y

