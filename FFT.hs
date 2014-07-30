import Data.Complex
--TODO write a function to turn [Float]->[Complex Float]
fft_recur :: [Complex Float] -> [Complex Float]
fft_recur [a] = [a :+ 0.0]
fft_recur arr = 
    let a = fft_split arr in fft_recur (fst a) + x* fft_recur (snd a)

    -- trying to figure out where to do the recursion, probably after 'split = ...'
    let split = fft_split arr
        even = (fst split)++(fst split) 
        odd = (snd split)++(snd split) 
        roots = roots_gen (length even) 
        
    in zipWith (+) even (zipWith (*) odd roots)
       


fft_split :: [Float] -> ([Float],[Float])
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

