import Data.Complex

fft_recur :: [Float] -> [Complex Float]
fft_recur [a] = [a :+ 0.0]
--fft_recur arr = 
  --  let a = split arr in FFT_recur first a + x* FFT_recur second a

fft_split :: [Float] -> ([Float],[Float])
fft_split [] = ([],[])
fft_split [x] = ([x],[])
fft_split (x:y:xs) = (x:xp, y:yp) where (xp, yp) = fft_split xs


roots_gen :: Float -> [Complex Float]
roots_gen n = helper n n
    where
        helper n 1 = [1 :+ 0.0] :: [Complex Float]
        helper n i = 
	    let k = exp(((2*pi)/(n :+ 0.0))*(0.0:+1.0))
	        new_list = helper n (i-1)
            in (k * head new_list) : new_list

