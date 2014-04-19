module PairwiseAlignment(DnaBase(..), fromString, calcF) where
data DnaBase = A|T|G|C deriving (Eq, Read, Show)
type Dna = [DnaBase]

matchScore = 2
misMatchScore = -1
gapCost = 2

-- | 文字列からDna配列を生成
-- >>> fromString "ATGC"
-- [A,T,G,C]
fromString :: String -> Dna
fromString = foldr(\d acc -> (read [d]::DnaBase) : acc) []

-- | 二つの配列の最適コストF(i,j)を計算する
-- >>> calcF [G, A, T, T, A] [G, A, A, T, T, C]
-- 5
calcF :: Dna -> Dna -> Int
calcF [] ys = - length ys
calcF xs [] = - length xs
calcF xs ys = maximum [noGap xs ys, xGap xs ys, yGap xs ys] 

-- マッチorミスマッチ
noGap :: Dna -> Dna -> Int
noGap (x:xs) (y:ys)
  | x == y = matchScore    + calcF xs ys
  | otherwise = misMatchScore + calcF xs ys

-- xギャップ
xGap :: Dna -> Dna -> Int
xGap (x:xs) ys =  -gapCost + calcF xs ys

-- yギャップ
yGap :: Dna -> Dna -> Int
yGap xs (y:ys) = -gapCost + calcF xs ys