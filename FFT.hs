module FFT where

import Data.Complex
import Data.List
import GHC.Float

fft :: [Complex Float] -> [Complex Float]
fft [a] = [a]
fft arr = 
    let split = fft_split arr
    	even_coef = fft (fst split)
    	odd_coef = fft (snd split)
        even_doubled = (even_coef)++(even_coef) 
        odd_doubled = (odd_coef)++(odd_coef) 
        roots = roots_gen (fromIntegral $ length even_doubled :: Float) 
    in zipWith (+) even_doubled (zipWith (*) odd_doubled roots)

fft_split :: [Complex Float] -> ([Complex Float],[Complex Float])
fft_split [] = ([],[])
fft_split [x] = ([x],[])
fft_split (x:y:xs) = (x:xp, y:yp) where (xp, yp) = fft_split xs

roots_gen :: Float -> [Complex Float]
roots_gen n = reverse (helper n n)
    where
        helper n 1 = [1 :+ 0.0] :: [Complex Float]
        helper n i = 
	    let k = exp(((2*pi)/(n :+ 0.0))*(0.0:+1.0))
	        new_list = helper n (i-1)
            in (k * head new_list):new_list

complexify :: [Float] -> [Complex Float]
complexify arr = map (:+ 0.0) arr 

decomplexify :: [Complex Float] -> [Float]
decomplexify arr = zipWith (square_of_sum) real imaginary 
    where real = map (**2) $ map realPart arr
          imaginary = map (**2) $ map imagPart arr

square_of_sum :: Float -> Float -> Float
square_of_sum a b = (a+b)**(0.5)

row_apply :: ([Complex Float]->[Complex Float]) -> [[Complex Float]] -> [[Complex Float]]
row_apply func rows = map func rows


ifft :: [Complex Float] -> [Complex Float]
ifft arr = map ( / a ) $ map conjugate $ fft $ map conjugate arr
	    where a = (fromIntegral $ length arr) :+ 0.0

fft2 :: [[Float]] -> [[Complex Float]]
fft2 image = transpose $ row_apply fft $ transpose $ row_apply fft (map complexify image)

ifft2 :: [[Complex Float]] -> [[Complex Float]]
ifft2 frequencies = transpose $ row_apply ifft $ transpose $ row_apply ifft (frequencies)
