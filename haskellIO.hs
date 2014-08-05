--nums.txt
import Control.Monad

sumEx :: [[Float]] -> [Float]
sumEx [x] = [sum x]
sumEx (x:xs) = sum x : sumEx xs

main = do
	contents <- readFile "nums.txt"
	let nums = read contents :: [[Float]]
	let nums2 = sumEx nums
	putStrLn $ show nums2
