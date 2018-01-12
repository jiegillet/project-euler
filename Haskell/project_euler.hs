import Data.List
import Data.Char (ord, chr)
import Data.Function (on)
import Data.Bits (xor)
import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Ratio
import System.IO
import Control.Applicative
import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Heap as H

euler1 n = sum [x | x<-[1..n-1], mod x 5 == 0 || mod x 3 == 0]

fib :: (Integral a) => a -> [a]
fib 1 = [1]
fib 2 = [2,1]
fib n = (f!!0 + f!!1):f
  where f = fib (n-1)

euler2 = sum [x | x <- fib 50 , x <= 4*10^6 , even x]

sieve :: (Integral n) => [n] -> [n]
sieve [] = []
sieve (n0:nx) = n0:(sieve [x | x <- nx, mod x n0 /= 0 ])

-- returns a list of primes smaller than n
primes_upto :: (Integral n) => n -> [n]
primes_upto n = sieve (2:[3,5..n])

-- Better implementation below around euler23
-- prime_factors :: (Integral n) => (n, [n], [n]) -> (n, [n], [n])
-- prime_factors (n, factors, sieve@(s0:s))
--   | n==1      = (n, factors, (s0:s))
--   | otherwise = prime_factors (div n (d^mult), d:factors, [x | x <- s, x >= d || mod x d /= 0 ])
--   where d = head [x | x <- sieve, mod n x ==0]
--         mult = (head [x | x <- [1..], mod n (d^x) /= 0]) -1

-- euler3 = maximum a
--    where (_,a,_)=(let n=600851475143 in prime_factors (n, [], 2:[3,5..n]))

euler3 = last $ prime_factors 600851475143

euler4 = maximum [ x*y | x <- [100..999], y <- [100..999], reverse (show (x*y)) == (show (x*y)) ]

euler5 = head [x | x <- [n,2*n..], sum [ mod x y | y <- lst ] == 0 ]
  where n = product (primes_upto 20)
        lst = let notPrime = not.(`elem`(primes_upto 20)) in filter notPrime [2..20]

--euler6 = (sum [1..100])^2 - (sum [x^2 | x <- [1..100]])
euler6 = (sum x)^2 - (sum $ map (^2) x)
  where x =[1..100]

-- returns a list of the first n primes
-- n_primes :: (Integral n) => n -> [n] -> [n]
-- n_primes n pr
--   | (fromIntegral (length pr))==n = pr
--   | otherwise = n_primes n ((head [x | x <- [p,p+2..], product [mod x k | k <- pr] /= 0]):pr)
--   where p = (head pr)+2

euler7 = x!!10000
  where x = sieve (2:[3,5..])

big = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
euler8 = maximum [ product [ read [big !! s] :: Int | s <- [indx..indx+len-1] ] | indx <- [0..(length big)-len]]
  where len = 13

euler9 = head [ a*b*(1000-a-b) | a<-[1..1000], b<-[a+1..1000-a], a^2+b^2==(1000-a-b)^2 ]

-- Too long :(
euler10 = sum $ takeWhile (<2000000) primes


grid=["08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08","49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00", "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65","52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91", "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80","24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50", "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70","67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21", "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72","21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95", "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92","16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57", "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58","19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40", "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66","88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69", "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36","20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16", "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54","01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"]
--str_to_mat :: [String] -> [[Int]]
--str_to_mat g = [[read [str!!k | k<-[3*i..3*i+1]]  :: Int | i <- [0..length g - 1] ] | str <- g]

euler11 = maximum (
  [ product [ g!!(i+k)!!j | k<-[0..3] ] | i <- [0..16], j <- [0..19]] ++
  [ product [ g!!i!!(j+k) | k<-[0..3] ] | i <- [0..19], j <- [0..16]] ++
  [ product [ g!!(i+k)!!(j+k) | k<-[0..3] ] | i <- [0..16], j <- [0..16]] ++
  [ product [ g!!(i+3-k)!!(j+k) | k<-[0..3] ] | i <- [3..16], j <- [0..16]] )
  where g =  map (map (read::String->Int) . words ) grid

tr_numbers :: (Integral n) => n -> [n]
tr_numbers 1 = [1]
tr_numbers n = (n+head t):t
  where t = tr_numbers (n-1)

--euler12 = head [ tr | tr <- triangles, length [d | d<-[1..tr], mod tr d == 0] > 500 ]
--  where triangles =  [ div (x*(x+1)) 2 | x<-[1..] ]

isTriangular :: (Integral a) => a -> Bool
isTriangular n = floor x * ceiling x == 2*n && floor x /= ceiling x
  where x =  sqrt $ fromIntegral (2*n)

euler12 = head $ filter (\tr -> length (filter (\x -> 0==mod tr x) [1..floor $ sqrt (fromIntegral tr :: Double)]) >250 ) $ scanl1 (+) [1..]

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n: collatz (div n 2)
  | odd n  = n: collatz (3*n+1)


euler13 = do
  txt <- readFile "../p13.txt"
  let s = sum $ map read $ lines txt
  print $ take 10 $ show s

-- slightly too slow (1'16'' compiled) come back to it later
--euler14 = maximumBy (\x y -> compare (length x) (length y)) (map collatz [1..1000000])

collatz_step :: Int -> Int
collatz_step n
  | even n = div n 2
  | odd n  = 3*n + 1

collatz_length :: Int -> State (M.IntMap Int) (Int, Int)
collatz_length 1 = return (1, 1)
collatz_length n = do
  m <- get
  if M.member n m
    then return (m M.! n, n)
    else do
      (l, _) <- collatz_length $ collatz_step n
      modify (M.insert n (l+1))
      return (l+1, n)

euler14 = snd (maximum lengths)
  where lengths = evalState (mapM collatz_length [1..999999]) M.empty

fact :: (Num a, Ord a) => a -> a
fact n
  | n<=0 = 1
  | otherwise = n * fact (n-1)

euler15 = div (product [21..40]) (product [1..20])

euler16 = sum $ map (\x -> read [x]) $ show $ 2^1000

euler17 = length $ filter (/=' ') $ concat $  map readOutLoud [1..1000]
  where units = ["zero","one","two","three","four","five","six","seven","eight","nine"]
        teens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
        tens = ["zero","ten","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
        readOutLoud n
          | n < 10       = units!!n
          | n < 20       = teens!!(n-10)
          | n == 1000    = "one thousand"
          | mod n 100==0 = units!!(div n 100) ++ " hundred"
          | n > 99       = units!!(div n 100) ++ " hundred and " ++ (readOutLoud $ mod n 100)
          | mod n 10==0  = tens!!(div n 10)
          | otherwise    = tens!!(div n 10) ++ " " ++ (readOutLoud $ mod n 10)

euler18 = do
  contents <- readFile "../p18.txt"
  print $  pathmax contents

pathmax :: String -> Int
pathmax =  head . accum . format
  where format = map (map read . words) . lines
        accum = foldr1 (\x acc -> zipWith (+) x (maxnext acc))
        maxnext acc = zipWith max (init acc) (tail acc)

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show, Enum, Eq)
data Month = Jan | Feb | Mar | Apr | May | Jun |
             Jul | Aug | Sep | Oct | Nov | Dec deriving (Show, Enum, Eq)
type Date = (Int, Month, Int, Weekday)

tomorrow :: Date -> Date
tomorrow (y,m,d,wd)
  | m==Dec && d==31                  = (y+1,Jan,1,nextWeekday wd)
  | isLeap y && m==Feb && d==28      = (y,Feb,29,nextWeekday wd)
  | isLeap y && m==Feb && d==29      = (y,Mar,1,nextWeekday wd)
  | (lookup m dayInMonths) == Just d = (y,nextMonth m,1,nextWeekday wd)
  | otherwise                        = (y,m,d+1,nextWeekday wd)
    where nextWeekday wd = if wd==Sun then Mon else succ wd
          nextMonth m    = if  m==Dec then Jan else succ m
          isLeap n = (mod n 400)==0 || ((mod n 100)/=0 && (mod n 4)==0 )
          dayInMonths = zip [Jan ..] [31,28,31,30,31,30,31,31,30,31,30,31]

euler19 = length $ filter sun1 $ takeWhile (\(y,_,_,_)->y<2001) days
  where days = iterate tomorrow (1900, Jan, 1, Mon)
        sun1 (y,_,d,wd) = (d==1) && (wd==Sun) && (1900<y)

euler20 = sum $ map (\x -> read [x]) $ show $ fact 100

properDivisors :: Int -> [Int]
properDivisors n = 1 : propdiv n 2 (div n 2)
  where propdiv n i maxd
          | i>maxd      = []
          | mod n i ==0 = (nub [i , div n i ]) ++ propdiv n (i+1) (div n (i+1))
          | otherwise   = propdiv n (i+1) maxd

isAmicable :: Int -> Bool
isAmicable n = n == (sum $ properDivisors m) && (n/=m)
  where m = sum $ properDivisors n

euler21 =  sum $ filter isAmicable [1..9999]

charToInt :: Char -> Int
charToInt x = (ord x) - (ord 'A') + 1

euler22 = do
  contents <- readFile "../names.txt"
  let names = sort (read ("["++contents++"]") :: [String])
      score = sum $ zipWith (*) [1..] (map (sum . map charToInt) names)
  putStrLn $ show score
  mapM_ putStrLn names

-- sumDivisors :: Int -> Int
-- sumDivisors n = propdiv 2 n
--   where propdiv i n
--           | i>maxd      = 1
--           | mod n i ==0 = (prod i n) * (propdiv (i+1) (n `div` (i ^(mult i n))))
--           | otherwise   = propdiv (i+1) n
--         maxd = floor $ sqrt $ fromIntegral n
--         prod i n = last $ take (mult i n) $ iterate (\x -> x*i+1) 1
--         mult i n = length $ takeWhile (\x -> x>0 &&  mod n x==0) $ iterate (`div` i) n

-- Give out tuples of (prime factor, multiplicity)
prime_factors :: Int -> [(Int,Int)]
prime_factors n
 | even n = let m = mult 2 n in (2,m): primeF 3 (div n (2^m))
 | otherwise = primeF 3 n
  where primeF d n
          | n==1 = []
          | d>(floor $ sqrt $ fromIntegral n) = [(n,1)]
          | mod n d == 0  = let m = mult d n in (d,m): primeF (d+2) (div n (d^m))
          | otherwise     = primeF (d+2) n
        mult d n = pred $ length $ takeWhile (>0)
                        $ iterate (\x -> if mod x d==0 then div x d else 0) n

sumDivisors :: Int -> Int
sumDivisors = foldl (\acc (p,a) -> acc * div (p^(a+1)-1) (p-1) ) 1 . prime_factors

isAbundant :: Int -> Bool
isAbundant n = 2*n < sumDivisors n

-- abundantPrSieve :: [Int] -> [Int]
-- abundantPrSieve [] = []
-- abundantPrSieve (n:ns)
--   | isAbundant n = n :abundantPrSieve b
--   | otherwise    = abundantPrSieve ns
--     where b = filter (\x -> mod x n /=0) ns
--main = do print euler23

-- compiled, takes 30 seconds
euler23 = sum $ M.keys $ foldl (\im n -> M.delete n im) lst s
  where s =[ sum x| x <- sequence (replicate 2 abundant), head x <= head (tail x) ]
        abundant = filter isAbundant [12..lim]
        lst = M.fromList $ zip [1..lim] $ repeat 0
        lim = 28123

--euler24 = ([ x | x<-sequence (replicate 10 ['0'..'9']), nub x == id x])!!999999

findLexPerm :: (Eq a) =>  [a] -> Integer -> [a]
findLexPerm [l] _ = [l]
findLexPerm l indx = element : findLexPerm (delete element l) (mod indx size)
  where element = l!!fromIntegral (div indx size)
        size = product [1.. fromIntegral (length l -1)]

euler24 = findLexPerm ['0'..'9'] 999999

euler25 = (999 * log 10 + log (sqrt 5)) / log phi
  where phi = (1 + sqrt 5)/2

recurringFraction :: Int -> [Int] -> Int  -> (Int, Int)
recurringFraction num lst den
  | num  < den = recurringFraction (10*num) (0:lst) den
  | r == 0     = (0,den)
  | elem r lst = (1 + length (takeWhile (/=r) lst), den)
  | otherwise  = recurringFraction (10*r) (r:lst) den
  where r = mod num den

euler26 = snd $ maximum $ map (recurringFraction 1 []) [2..999]

euler27 = snd $ maximum [ (l a b, a*b) | a<-as, b<-bs ]
  where bs = (\x -> x ++ map negate x) $ primes_upto (1000)
        as = [-999,-997..999]
        pr = let n = maximum bs in primes_upto (10000)
        l a b = length $ takeWhile (`elem` pr) $ map (\n->n^2+a*n+b) [0..]

euler28 =  foldl (\acc n -> acc + 4*n^2-6*n+6) 1 [3,5..1001]

euler29 = length $ nub $ (^) <$> [2..100] <*> [2..100]

-- euler30 = filter (\n -> n==digitComb n ) [2..1000000]
--   where digitComb = sum . map (\d -> (read [d])^5) . show
euler30 = sum $ concat $ map (map (sum . map (^5)) . filter digitComb . list) [3..7]
  where ascending x = and $ zipWith (<=) (init x) (tail x)
        list n = filter ascending $ sequence $ replicate n [0..9]
        digitComb l = concat (map show l) == sort (show $ sum $ map (^5) l)

waysToSum :: [Integer] -> Integer -> Integer
waysToSum [c] s = if mod s c == 0 then 1 else 0
waysToSum (c:cs) s = sum $ map (\n -> waysToSum cs (s-n*c)) [0..div s c]

euler31 = waysToSum coins 200
  where coins = [200,50,100,20,10,5,2,1]

euler32 = sum $ nub $foldl mmp [] $ map (findLexPerm [1..9]) [0..fact 9 - fact 8 - 1]
  where mmp acc l = case pr $ p144 l of
                   True -> (last $ p144 l ): acc
                   False -> case pr $ p234 l of
                              True -> (last $ p234 l) : acc
                              False -> acc
        pr = (\x -> x!!0*x!!1==x!!2)
        p144 x = let (a,b) = splitAt 4 $ tail x
                  in head x : map lstToInt [a, b]
        p234 x = let (a,b') = splitAt 2 x
                     (b,c)  = splitAt 3 b'
                  in map lstToInt [a, b, c]
        lstToInt = foldl1 ((+).(*10))

euler33 = product [ up%down | up <- [1..9], down <- [1..9], c <- [1..9],
             down > up, cross up down c || cross down up c]
  where cross a b c = a*(10*b+c)==b*(10*c+a)

euler34 = sum $ map (sum . map (fcts!!)) $ concat $ map (filter digitComb . list) [2..7]
  where ascending x = and $ zipWith (<=) (init x) (tail x)
        list n = filter ascending $ sequence $ replicate n [0..9]
        digitComb l = sort ( concat (map show l)) == sort (show $ sum $ map (fcts!!) l)
        fcts = map fact [0..9]

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = null $ tail $ primeFactors n

primeFactors :: Integer -> [Integer]
primeFactors n = pfac n primes
  where pfac n (p:ps)
          | p*p>n       = [n]
          | mod n p ==0 = p: pfac (div n p) (p:ps)
          | otherwise   = pfac n ps

euler35 = length $ 2:3:5:7:(filter circPrime $ filter digits pr)
  where digits = and . map (`elem` "1379") . show
        circPrime = and . map (`elem` pr) . map read . init . lettRot . show
        lettRot n = map (take $ length n) $ map (flip drop $ cycle n) [1..length n]
        pr = drop 4 $ takeWhile (<1000000) primes

euler36 = sum $ filter palindromeBin $ filter palindromeDec [1,3..999999]
  where palindromeDec n = show n == (reverse $ show n)
        palindromeBin n = let x = decToBin n in x == reverse x
        decToBin 0 = []
        decToBin n = let (q,r) = quotRem n 2 in r: decToBin q

euler37 = sum $ take 11 $ filter truncable $ drop 4 primes
  where truncable = and . map isPrime . map read . bits . show
        bits p = nub (init $ tail $ inits p) ++ (init $ tail $ tails p)
        isPrime n = elem n $ takeWhile (<=n) primes

euler38 = maximum $ filter pandigital $ map (conc.prod) ([91..98]++[912..987]++[9123..9876])
  where prod n = take 4 $ iterate (+n) n
        conc = foldl (\acc n -> acc ++ if length acc<9 then show n else "") ""
        pandigital = (==) ['1'..'9'] . sort

-- in ghci, takes 90 seconds
euler39 = snd $ maximum $ map numTr [2,4..1000]
  where numTr p = (length [ 1 | a<-[1..div p 3],
                                b<-[a..div p 2],
                                a^2+b^2==(p-a-b)^2], p)
-- Compiled, takes 43 seconds
-- main = do print euler39

euler40 = product $ map (\n -> ord n - 48) $ map (number!!) powers
  where number = concat $ map show $ [0..]
        powers = take 7 $ iterate (*10) 1

euler41 = head $ filter isPrime numbers
  where lstToInt = foldl1 ((+).(*10))
        perms n = map (lstToInt . findLexPerm [n,n-1..1]) [0..fact n-1]
        numbers = concat $ map perms [7,6]

euler42 = do
  txt <- readFile "../words.txt"
  let wrds = (read ("["++txt++"]") :: [String])
      wordToVal = foldl (\acc l -> acc + ord l - 64) 0
  print $ length $ filter (isTriangular.wordToVal) wrds

euler43 = sum $ map lstToInt $ filter substring perms
  where perms = map (findLexPerm [0..9]) [fact 9..fact 10-1]
        nums = map (lstToInt . take 3) . take 7 . tails . drop 1
        lstToInt = foldl1 ((+).(*10))
        substring = and . zipWith (\a b -> mod b a==0) primes . nums

isPentagonal :: Integer -> Bool
isPentagonal p = abs(n - (fromInteger $ round n))<1e-10
  where n = sqrt(fromInteger p *2/3+1/36)+1/6

euler44 = head [a-b | a <- pent, b <- takeWhile (<a) pent,
                  isPentagonal (a+b) && isPentagonal (a-b)]
  where pent = map (\ n-> n*(3*n-1) `div` 2) [1..10000]

isHexagonal :: Integer -> Bool
isHexagonal p = abs(n - (fromInteger $ round n))<1e-10
  where n = sqrt(fromInteger p /2+1/16)+1/4

euler45 = head $ filter (\n -> isPentagonal n && isHexagonal n) tr
  where tr = map (\ n-> n*(n+1) `div` 2) [286..]

euler46 = head $ filter (not.isGoldbach) $ filter (not.null.tail.primeFactors) [9,11..]
  where isGoldbach n = any (\p -> isTwiceSq $ n-p ) $ takeWhile (<n) $ tail primes
        isTwiceSq p = let n=sqrt(fromIntegral p /2) in n == (fromInteger $ floor n)

euler47 = fst $ head $ head $ dropWhile notcons $ map (take 4) $ tails $ factors
  where factors = zip <*> (map (nub.primeFactors)) $ [1..]
        notcons = not . and . map ((==) 4 . length . snd)

euler48 = flip mod (10^10) $ sum $ map (\n->n^n) [1..1000]

euler49 = concat $ map arSeq $ nub $ map perm pr
  where pr = takeWhile (<10000) $ dropWhile (<1000) primes
        perm x = filter (\n-> (sort $ show x)==(sort $ show n)) pr
        arSeq [a] = ""
        arSeq (a:[b]) = ""
        arSeq (a:b:c:xs) = if (b-a)==(c-b) then concat $ map show [a,b,c] else arSeq (b:c:xs)

allCuts :: [[a]] -> [[a]]
allCuts [] = []
allCuts [[]] = [[]]
allCuts l@([a]:ls) = l
allCuts l = l ++ (allCuts ((map tail l)++(map init l)))

consPrimes :: Integer -> ([Integer], [Integer]) ->  ([Integer], [Integer])
consPrimes lim x = last $ filter primeSum $ takeWhile limitedSum $ x:(newLists x)
  where primeSum = isPrime . sum . fst
        limitedSum = (>=) lim . sum . fst
        newLists (_, []) = []
        newLists (a, b) = let c=(head b: a, tail b) in c : newLists c

euler50 = sum $ fst $ maximumBy (compare `on` (length . fst)) $ cons
  where cons = map (consPrimes 1000000) starts
        starts = map (\n -> ([primes!!n], drop (n+1) primes)) [0..50]

-- not finished!!
-- euler51 = filter ((<=) 7 . length) $ map (filter (isPrime . read)) $ parse pairs
--   where pairs =  drop 3 $ take 4 $ iterate insertDigit $ map show [1..9]
--         insertDigit x@(x0:xs) = insertAt <$> [0..length (show x0)-1] <*> (map show [0..9]) <*> x
--         insertAt i x l = let (a,b)= splitAt i l in a++x++b
--         parse [] = []
--         parse x = let (a,b) = splitAt 9 x in a: parse b

euler52 = head $ filter permMult [1..]
  where permMult n = and $ map ((==) (sort $ show n).sort.show.(*) n) [2..6]

euler53 = length $ filter (>1000000) $ concat $ take 101 $ iterate next [1]
  where next l = let x= zipWith (+) (tail l) (init l) in [1] ++ x ++ [1]

euler55 = length . filter isLychrel $ [1..9999]
  where isLychrel = not . any isPal . take 50 . tail . iterate revAdd
        isPal n = (show n) == (reverse $ show n)
        revAdd n = n + (read $ reverse $ show n)

euler56 = maximum $ map sumDigits $ (^) <$> [2..99] <*> [1..99]
  where sumDigits = sum . map (\n -> read [n]) . show

euler57 = length $ filter bigNum $ take 1000 $ iterate (\n ->1+ 1/(1+n)) $ 3%2
  where bigNum r = (length $ show $ numerator r)>(length $ show $ denominator r)

euler58 = s
  where corners = map (\n -> [n^2-(n-1),n^2-2*(n-1),n^2-3*(n-1)])  [5,7..]
        numPr = scanl nextgen (3,5,3) corners
        nextgen (p,n,s) l = (p+(length $ filter isPrime l),n+4,s+2)
        (_,_,s) = head $ dropWhile (\(p,n,s)->10*p>=n) numPr

euler59 = do
  contents <- readFile "../p059_cipher.txt"
  let encrypted = read $ "[" ++ contents ++ "]" :: [Int]
      grouped = group . sort . zip (cycle [1,2,3])
      grouped2 = sort . map (\l@((g,ch):xs) -> (g,length l,ch))
      spaces = map ((\(_,_,e)->e).last) . groupBy (\(a,_,_) (b,_,_) -> a==b)
      key = map (xor (ord ' ')) . spaces . grouped2 . grouped $ encrypted
      decrypted = zipWith xor (cycle key) encrypted
  print $ sum decrypted

euler60 = sum $ head $ primePairSet 5 $ takeWhile (<10000) $ tail primes
  where concatPr a b = all (isPrime.read) [show a ++ show b, show b ++ show a]
        primePairSet _ [] = []
        primePairSet 1 p = [[head p]]
        primePairSet n (p:ps) = map (p:) next ++ primePairSet n ps
         where candidates = filter (concatPr p) ps
               next = primePairSet (n-1) candidates

isSquare :: Integer -> Bool
isSquare p = abs(n - (fromInteger $ round n))<1e-10
  where n = sqrt(fromInteger p)

isHeptagonal :: Integer -> Bool
isHeptagonal p = abs(n - (fromInteger $ round n))<1e-10
  where n = sqrt(fromInteger p*2/5+9/100)+3/10

isOctogonal :: Integer -> Bool
isOctogonal p = abs(n - (fromInteger $ round n))<1e-10
  where n = sqrt(fromInteger p/3+1/9)+1/3

euler61 = sum.tail.head.head.dropWhile null.map findCyclic.permutations $ numz
   where figurate = [isTriangular, isSquare, isPentagonal, isHexagonal, isHeptagonal, isOctogonal]
         numz =  getZipList $ filter <$> ZipList figurate <*> pure [1000..9999]
         coDigits (a:b:x)= let sp=splitAt 2.show in (snd $ sp b)==(fst $ sp a)
         findCyclic l = filter (\l->(head l)==(last l)) $ foldl findNext (map (:[]) $ last l) l
         findNext a b = filter coDigits $ (:) <$> b <*> a

euler62 = evalState (cubicPerm 1) Map.empty

cubicPerm :: Integer -> State (Map.Map String [Integer]) Integer
cubicPerm n = do
  m <- get
  let s = sort $ show $ n^3
  if not $ Map.member s m
    then do modify $ Map.insert s [n]
            cubicPerm (n+1)
    else if (length $ m Map.! s)==4
              then return $ (^3) $ last $ m Map.! s
              else do modify $ Map.adjust (n:) s
                      cubicPerm (n+1)

euler63 = sum $ takeWhile (>0) $ map num [1..]
  where num n = length $ takeWhile (<10^n) $ dropWhile (<10^(n-1)) $ map (^n) [1..]

euler64 = length $ filter odd $ map period [1..10000]
  where fi = fromInteger
        period n = let z=floor.sqrt.fi $ n
                   in if z^2 == n then 0
                      else contFrPeriod n (next n (z,1,z)) []
        next n (a,num,diff) =
         let (num', m) = let x=num%(n-diff^2) in (denominator x, numerator x)
             a' = div (floor $ (fi m) * (sqrt (fi n) + fi diff)) num'
             diff' = a'*num' - diff
         in (a',num',diff')
        contFrPeriod n ai@(a,_,_) lst
          | elem ai lst = length lst
          | otherwise   = contFrPeriod n (next n ai) (ai:lst)

euler65 = sn $ foldr1 (\n f -> n + 1/f) contFr
  where contFr = take 100 $ 2 : (concat $ map (\n->[1,2*n,1]) [1..])
        sn = sum . map (read . pure) . show . numerator

euler66 = snd $ maximum $ zip (map sol ds) ds
  where
  fi = fromInteger
  ds = [1..1000] \\ map (^2) [1..100]
  sol d = numerator $ head $ filter (isSol d) $ map evalContFr $ tail $ inits $ contFr d
  isSol d = (\n-> (numerator n)^2 - d*(denominator n)^2 ==1)
  evalContFr = foldr1 (\n f -> n + 1/f) . map fromInteger
  contFr k = let z=floor.sqrt.fi $ k in map (\(n,_,_)-> n) $ iterate (next k) (z,1,z)
  next n (a,num,diff) =
   let (num', m) = let x=num%(n-diff^2) in (denominator x, numerator x)
       a' = div (floor $ (fi m) * (sqrt (fi n) + fi diff)) num'
   in (a',num',a'*num' - diff)

euler67 = do
  contents <- readFile "../triangle.txt"
  print $  pathmax contents

chooseKinN :: Integer -> [a] -> [[a]]
chooseKinN k n@(n0:ns)
  | k < 1 = []
  | k == 1 = map (:[]) n
  | k > (fromIntegral $ length n) = []
  | otherwise = (map (n0:) $ chooseKinN (k-1) ns)  ++ chooseKinN k ns

euler68 = head $ filter ((==) 16.length.concatMap show.concat) gongs
  where
  gongs = filter magic $ makeGongs 5 [10,9..1]
  makeGongs n lst = concatMap
    (\l -> concatMap (makeLines l) $ chooseKinN n (lst \\ l)) $ chooseKinN n lst
  makeLines l k = format <$> (map (last l:) $ permutations $ init l) <*> permutations k
  format l k = zipWith3 (\a b c -> [a,b,c]) l k $ tail $ cycle k
  magic g = all ((==) (sum $ head g) . sum) g

euler69 = last $ takeWhile (<=1000000) $ scanl1 (*) primes

totient :: Integer -> Integer
totient n = foldl (\a p -> a-(div a $ head p)) n $ group $ primeFactors n

euler70 = sol
  where (_,sol,_) = minimum $ filter isPerm $ map tot nums
        isPerm (_,n,t) = (sort $ show n ) == (sort $ show t)
        tot n = let t = totient n in (n%t, n, t)
        nums = [x*y | x<-pr, y<-pr, x<y, x*y<10000000]
        pr = drop 300 $ take 500 primes

euler71 = numerator.last.takeWhile (<3%7).sort $[3*d `div` 7%d | d<-[1..1000000]]

euler72 = sum $ map totient [2..1000000]

euler73 = length [0 | d<-[1..12000], n<-[(d `div` 3)..(d `div` 2)],
                      n%d>1%3, n%d<1%2, gcd n d ==1 ]

-- main = do print euler74
euler74 = length $ filter (==60) $ evalState (mapM digitFacChain [1..999999])
           (M.fromList [(871,2),(872,2),(45361,2),(45361,2),(169,3),(363601,3),(1454,3)])

fac = Map.fromList $ zip ['0'..'9'] $ map fact [0..9]

digitFacChain :: Int -> State (M.IntMap Int) Int
digitFacChain n = do
  if digitFact n == n
    then do modify $ M.insert n 1
            return 1
    else do m <-get
            if M.member n m
              then return $ m M.! n
              else do l <- digitFacChain $ digitFact n
                      modify $ M.insert n (l+1)
                      return $ l+1
  where digitFact = sum . map (fac Map.! ) . show

euler75 = length $ filter (==1) $ M.elems $ all_pers perimeters M.empty
  where
  all_pers [] m = m
  all_pers (p:ps) m = all_pers ps $ case M.lookup p m of
                                       Just _ -> M.adjust (+1) p m
                                       Nothing -> M.insert p 1 m
  perimeters = concatMap (\n->takeWhile (<=1500000) $ iterate (+n) n) $ pythTriple (3, 4, 5)
  pythTriple t@(a, b, c) = let s=a+b+c in
    if s>1500000 then [] else s:(concatMap pythTriple $ [ut,at,dt] <*> [t])
  ut (a, b, c) = (a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c)
  at (a, b, c) = (a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c)
  dt (a, b, c) = (-a+2*b+2*c, -2*a+b+2*c, -2*a+2*b+3*c)

-- main = do print euler76
euler76 = waysToSum [99,98..1] 100

euler77 = snd $ head $ dropWhile ((>=) 5000 . fst) nums
  where nums = map (\n->(waysToSum (takeWhile (<n) primes) n, n)) [10..]

euler78 = snd $ head $ dropWhile ((/=) 0 . flip mod 1000000 . fst) $ tail $ zip state [1..]
  where state = evalState (mapM partitionP [1..]) (M.fromList [(-1,0),(0,1)])

partitionP :: Int -> State (M.IntMap Integer) Integer
partitionP n = do
  m <- get
  let p = sum $ map (\k-> (-1)^(k+1) *
                      (m M.! (max (-1) $ n- k*(3*k-1) `div` 2 )
                      + m M.! (max (-1) $ n- k*(3*k+1) `div` 2 )))
                                         [1..floor $ sqrt $ fromIntegral n]
  modify $ M.insert n p
  return p

passcode :: [String] -> String
passcode c = map snd $ reverse $ sort $ map compile . nub . concat $ c
  where compile d =  (length $ nub $ concatMap (dropWhile (/=d)) c, d)

euler79 = do
  codes <- readFile "../keylog.txt"
  print $ passcode $ lines codes

sqrt' :: (Integer, Integer) -> Integer -> (Integer, Integer)
sqrt' (p, r) n = (p*10+x, c-y)
  where c = 100*r + n
        y = x*(20*p+x)
        x = solve $ if p==0 then 6 else div c (20*p)
        solve guess
          | guess*(20*p+guess) > c = solve (guess-1)
          | (guess+1)*(20*p+guess+1) <= c = solve (guess+1)
          | otherwise = guess

euler80 = sum $ map (sum . listDigits . sqDigits) ([1..100] \\ map (^2) [1..10])
  where sqDigits n = fst $ foldl sqrt' (0, 0) $ take 100 $ format n ++ repeat 0
        format 0 = []
        format n = format (div n 100) ++ [mod n 100]
        listDigits = map (\x -> read [x]) . show

-- -- Super slow :( -- because I'm keeping track of every path instead of nodes
-- --minPathSum :: Int -> Int -> M.IntMap Integer ->  Integer
-- minPathSum height width m = floor $ go (1/0) $ H.singleton (m M.! 0, 1, 1, [m M.! 0])
--   where go min_p h
--           | s > min_p = min_p
--           | i==height && j==width = go s h'
--           | i==height = go min_p $ H.insert (s+right, i, j+1, right:p) h'
--           | j==width  = go min_p $ H.insert (s+down, i+1, j, down:p) h'
--           | otherwise = go min_p $ H.insert (s+right, i, j+1, right:p)
--                                  $ H.insert (s+down, i+1, j, down:p) h'
--             where Just ((s, i,  j, p) , h') = H.viewMin h
--                   right = m M.! ((i-1)*width+j)
--                   down  = m M.! (i*width+(j-1))

euler81' :: Int -> M.IntMap Integer ->  Integer
euler81' side m = head $ foldl accum [m M.! 0] [2..(2*side-1)]
  where accum d i
          | i<=side   = zipWith (+) (get_diag i) ([head d] ++ (mins d) ++ [last d])
          | otherwise = zipWith (+) (get_diag i) (mins d)
        get_diag d=map (\x -> m M.! ((d-x)*side+(x-1))) [max 1 (d-side+1)..min d side]
        mins x = zipWith min (init x) (tail x)

euler81 = do
  m <- readFile "../matrix.txt"
  print $ euler81' 80 $ M.fromList $ zip [0..] $ concatMap (map read . splitOn ",") $ lines m

dijkstra :: [Int] -> [Int] -> [Int] -> M.IntMap Int -> (Int -> [Int]) -> [(Int, [Int])]
dijkstra nodes source target weights neighbors = step (heap, dist, prev) where
  heap = H.fromListWith ((<=) `on` (dist M.!)) nodes
  dist = M.fromList $ dist0 ++ [(i, weights M.! i) | i<-source]
  dist0 = zip nodes $ repeat (maxBound :: Int)
  prev = M.fromList $ zip nodes $ repeat (-1)
  step (h, d, p)
    | H.null h  = let minP = minimum $ map (d M.!) target
                      path (-1) = []
                      path n = n:path (p M.! n)
                  in map (\x->(minP, path x)) $ filter (\i -> (d M.! i)==minP) target
    | otherwise = step stuff where
    (u, h') = H.popMin h
    stuff = foldl replaceAll (h', d, p) $ filter shortDist $ neighbors u
    shortDist v = d M.! u + weights M.! v < d M.! v
    replaceAll (hf, df, pf) v =
      let d' = M.adjust (\_ -> df M.! u + weights M.! v) v df
      in (H.replaceWith ((<=) `on` (d' M.!)) v v hf, d', M.adjust (\_->u) v pf)

euler82 = do
  m <- readFile "../matrix.txt"
  let side = length $ lines m
      nodes = [0..side^2-1]
      source = map (*side) [0..side-1]
      target = map (\i->i*side-1) [1..side]
      weights = M.fromList $ zip nodes $ concatMap (map read . splitOn ",") $ lines m
      neighbors u
        | j==side-1 = []
        | j==0      = [i*side + j+1]
        | i==side-1 = [i*side + j+1, (i-1)*side + j]
        | i==0      = [i*side + j+1, (i+1)*side + j]
        | otherwise = [i*side + j+1, (i+1)*side + j, (i-1)*side + j]
        where (i, j) = quotRem u side
  print $ fst $ head $ dijkstra nodes source target weights neighbors

euler83 = do
  m <- readFile "../matrix.txt"
  let side = length $ lines m
      nodes = [0..side^2-1]
      source = [0]
      target = [side^2-1]
      weights = M.fromList $ zip nodes $ concatMap (map read . splitOn ",") $ lines m
      neighbors u
        | u==side^2-1 = []
        | u==0        = [i*side + j+1, (i+1)*side + j]
        | u==side-1   = [i*side + j-1, (i+1)*side + j]
        | u==side^2-side= [i*side + j+1, (i-1)*side + j]
        | u==side-1   = [i*side + j-1, (i+1)*side + j]
        | j==0        = [i*side + j+1, (i+1)*side + j, (i-1)*side + j]
        | j==side-1   = [i*side + j-1, (i+1)*side + j, (i-1)*side + j]
        | i==0        = [i*side + j+1, i*side + j-1, (i+1)*side + j]
        | i==side-1   = [i*side + j+1, i*side + j-1, (i-1)*side + j]
        | otherwise   = [i*side + j+1, (i+1)*side + j, (i-1)*side + j, i*side + j-1]
        where (i, j) = quotRem u side
  print $ fst $ head $ dijkstra nodes source target weights neighbors

--euler84: Monopoly

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob (map (\(e, p) -> (f e, p)) xs)

instance Applicative Prob where
  pure x = Prob [(x,1%1)]
  Prob fs <*> Prob xs = Prob $ twoMult <$> fs <*> xs
    where twoMult (f, pf) (x, px) = (f x, pf * px)

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap extract xs
  where extract (Prob ys, px) = map (\(e,py) -> (e, py*px)) ys

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

nubProb :: (Eq a) => Prob a -> Prob a
nubProb (Prob xs) = Prob (nubP xs)
  where nubP []          = []
        nubP ((e, p):xs) = let (a, b) = partition ((==) e . fst) xs
                           in (e, p + sum (map snd $ a)) : nubP b

euler84 = cleanup $ getProb $ last $ take 25 $ iterate turn board
  where board = Prob $ zip [0..39] $ repeat (1%40)
        d = 4
        die = Prob $ zip [1..d] (repeat (1%d))
        rolldice i = fmap (\d -> mod (d+i) 40) $ nubProb $ (+) <$> die <*> die
        jail i = Prob [(case i of 30 -> 10; _ -> i ,1%1)]
        tripleDouble i = Prob [(10,(1%d)^3), (i,1-(1%d)^3)]
        chest i
          | i==2 || i==17 || i==33 = Prob [(10,1%16),(0,1%16),(i,14%16)]
          | otherwise              = return i
        chance i
          | i==7 || i==22 || i==36 = Prob [(10,1%16),(0,1%16),(11,1%16),(i,6%16),
              (case i of 7-> 15; 22 -> 25; 36 -> 5, 2%16),(24, 1%16),(39, 1%16),
              (case i of 7-> 12; 22 -> 28; 36 -> 12, 1%16),(5, 1%16),(i-3, 1%16)]
          | otherwise              = return i
        turn b = nubProb $ b >>= rolldice >>= tripleDouble >>= jail >>= chest >>= chance
        cleanup = concat.take 3.map (\i -> if i<10 then "0"++show i else show i)
                           . reverse . map fst . sortBy (compare `on` snd)

euler85 = let t=2000000 in  product $ minimumBy (compare `on` (r t)) $ makeRect t
  where makeRect t = takeWhile (\[n,m]->n<=m) $ concatMap (estim t) [1..]
        estim t n = let m=(sqrt((n^2+n+16*t)/(n^2+n))-1)/2
                    in [[round n, floor m], [round n, ceiling m]]
        r t [m,n] = abs $ m*n*(m+1)*(n+1) `div` 4 - round t



euler86 = 1 + binarySearch 1000 5000 (\m->(countUpTo m (1, [3, 4, 5]))-1000000) 1
  where
  countUpTo m (n, t)
    | a>m || (a+b)>3*m = 0
    | n>1 = numSol + countUpTo m (n+1, t)
    | otherwise = numSol + (sum $ map (countUpTo m) $ (2, t) : ([ut,at,dt] <*> [t]))
    where [a,b,_] = sort $ map (*n) t
          numSol = (if b<=m then div a 2 else 0)
                   +(if div b 2>=b-a then div b 2 -(b-a-1) else 0)
  ut [a, b, c] = (1, [a-2*b+2*c, 2*a-b+2*c, 2*a-2*b+3*c])
  at [a, b, c] = (1, [a+2*b+2*c, 2*a+b+2*c, 2*a+2*b+3*c])
  dt [a, b, c] = (1, [-a+2*b+2*c, -2*a+b+2*c, -2*a+2*b+3*c])
  binarySearch a b f eps
    | f a == 0 = a
    | b-a<=1 = a
    | (f a)*(f (div (a+b) 2)) > 0 = binarySearch (div (a+b) 2) b f eps
    | otherwise = binarySearch a (div (a+b) 2) f eps

euler87 = length $ evalState (filterM (primePowers lim) $ sequence nums) Map.empty
  where nums = map (\n->takeWhile (<lim) $ map (^n) primes) [2..4]
        lim = 50000000

primePowers :: Integer -> [Integer] -> State (Map.Map Integer Int) Bool
primePowers lim n = do
  m <- get
  if s>lim || Map.member (sum n) m
    then    return False
    else do modify (Map.insert (sum n) 1)
            return True
  where s = sum n

--euler88 =
