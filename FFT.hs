import Data.Complex

fft_recur :: [Complex Float] -> [Complex Float]
fft_recur [a] = [a]
fft_recur arr = 
    let split = fft_split arr
    	even_coef = fft_recur (fst split)
    	odd_coef = fft_recur (snd split)
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

row_fft :: [[Float]] -> [[Complex Float]]
row_fft rows = map fft_recur $ map complexify rows

fft2 :: [[Float]] -> [[Complex Float]]
fft2 image = transpose $ row_fft $ transpose $ row_fft image

--     = map FFT 1-d onto a 2-d array