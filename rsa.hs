import Data.Char

totient :: (Integral a) => a -> a
totient n = fromIntegral . length . filter (==1) . map (gcd n) $ [1..n-1]

minv :: (Integral a) => a -> a -> a
minv n p = (n ^ ((totient p) - 1)) `mod` p

messageToNumber :: String -> Integer
messageToNumber s = read . concat . map (show . ord) $ s :: Integer


primeFactors :: Integer -> [Integer]
primeFactors n = tryDivisors n [2..m]
	where m = floor . sqrt . fromIntegral $ n 

tryDivisors :: Integer -> [Integer] -> [Integer]
tryDivisors n [] = [n]
tryDivisors n (d:ds) =
	let n' = removeFactors n d
	in if n `mod` d == 0 then d:tryDivisors n' ds else tryDivisors n ds

-- given a number and a possible divisor, return that number with that divisor divided out.
removeFactors :: Integer -> Integer -> Integer
removeFactors n d
	| n `mod` d /= 0 = n
	| otherwise = removeFactors (n `div` d) d



