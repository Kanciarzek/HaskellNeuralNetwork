data Layer = Layer [[Double]] Layer | Output deriving (Show)

initializeNetwork :: [Int] -> Layer
initializeNetwork [a] = Output
initializeNetwork (a:b:rest) = Layer (makeBoard b (a+1)) (initializeNetwork (b:rest))

makeBoard :: Int -> Int -> [[Double]]
makeBoard h w = replicate h (replicate w 0)

computeOneLayer :: Layer -> [Double] -> [Double]
computeOneLayer Output list = list
computeOneLayer (Layer [] nextLayer) input = []
computeOneLayer (Layer coeffs nextLayer) input = [ sigmoid (dotProduct (head coeffs) input) ]
                                                    ++ computeOneLayer (Layer (tail coeffs) nextLayer) input

computeNetwork :: Layer -> [Double] -> [Double]
computeNetwork Output (one:list) = list
computeNetwork (Layer coeffs nextLayer) input = computeNetwork nextLayer (1:(computeOneLayer (Layer coeffs nextLayer) input))
                                                                                   
dotProduct :: [Double] -> [Double] -> Double
dotProduct [a] [b] = a*b
dotProduct (f:fstRest) (s:sndRest) = f*s + (dotProduct fstRest sndRest)

sigmoid :: Double -> Double
sigmoid x = 1/(1 + (exp (-x)))

sigmoidDiv :: Double -> Double
sigmoidDiv x = sigmoid x * (1 - (sigmoid x))

differenceList :: [Double] -> [Double] -> [Double]
differenceList [] [] = []
differenceList (f:fstRest) (s:sndRest) = (f-s) : differenceList fstRest sndRest

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

computeDelta :: [[Double]] -> [Double] -> [Double] -> [Double]
computeDelta [] previousDelta [] = []
--computeDelta (c:coeffRest) previousDelta (o:outputRest) | trace ("Delta" ++ show (c:coeffRest) ++ " " ++ show previousDelta ++ " " ++ show (o:outputRest)) False = undefined
computeDelta (c:coeffRest) previousDelta (o:outputRest) = ((dotProduct c previousDelta) * o *(1 - o)) : computeDelta coeffRest previousDelta outputRest

sumMult :: [Double] -> Double -> [Double] -> [Double]
sumMult [] d [] = []
sumMult (c:coeffLineRest) d (i:inputRest) = (c + d * i / 5) : sumMult coeffLineRest d inputRest

updateWeights :: [[Double]] -> [Double] -> [Double] -> [[Double]]
updateWeights [] [] input = []
updateWeights (c:coeffRest) (d:deltaRest) input = (sumMult c d input) : updateWeights coeffRest deltaRest input

backPropagateNetworkDelta :: Layer -> [Double] -> [Double] -> ([Double], Layer)
backPropagateNetworkDelta Output input expectedResult = (differenceList expectedResult input, Output)
backPropagateNetworkDelta (Layer coeffs nextLayer) input expectedResult = (delta, Layer (updateWeights coeffs (fst nextLayerResult) (1:input)) (snd nextLayerResult))
                                                                where output = computeOneLayer (Layer coeffs Output) (1:input)
                                                                      nextLayerResult = backPropagateNetworkDelta nextLayer output expectedResult
                                                                      delta = tail $ computeDelta (transpose coeffs) (fst nextLayerResult) (1:input)

backPropagateNetwork :: Layer -> [Double] -> [Double] -> Layer
backPropagateNetwork = ((snd.).).backPropagateNetworkDelta

oneHot :: Double -> Double -> [Double]
oneHot e max = (replicate (floor e) 0) ++ [1] ++ (replicate (floor (max - e)) 0)

trainNetwork :: Layer -> [[Double]] -> [Double] -> Layer
trainNetwork network [] expectedResults = network
trainNetwork network (i:initialDataRest) (e:expectedResultsRest) = 
                    trainNetwork (backPropagateNetwork network i (oneHot e 9)) initialDataRest expectedResultsRest

wordsComma :: String -> [String]
wordsComma s =  case dropWhile (==',') s of
                      "" -> []
                      s' -> w : wordsComma s''
                            where (w, s'') = break (==',') s'
                    
convertToDoubleLists :: [String] -> [[Double]]
convertToDoubleLists text = map (\(a:y) -> map (\x -> (read x :: Double)/255) (wordsComma y)) text

convertToResultList :: [String] -> [Double]
convertToResultList text = map (\(a:x) -> read [a] :: Double) text

convertToIntList :: String -> [Int]
convertToIntList = (map (\x -> (read x :: Int))).wordsComma

findMaxInList :: [Double] -> Int -> Int -> Double -> Int
findMaxInList [] iMax _ maxVal = iMax
findMaxInList (a:rest) iMax iCur maxVal = if a > maxVal then findMaxInList rest iCur (iCur+1) a else findMaxInList rest iMax (iCur+1) maxVal

printAndComputeNetwork :: [Double] -> Layer -> IO()
printAndComputeNetwork image network = do 
                    printImage image 0
                    putStrLn $ show $ findMaxInList (computeNetwork network (1:image)) 0 0 0
                    
main :: IO()
main = do
    content <- readFile "mnist_train.csv"
    topology <- readFile "topology.txt"
    testDigit <- readFile "test.txt"
    printAndComputeNetwork (head ((convertToDoubleLists.lines) testDigit)) (trainNetwork ((initializeNetwork.convertToIntList) topology) ((convertToDoubleLists.lines) content) ((convertToResultList.lines) content))

printImage :: [Double] -> Int -> IO()
printImage [] _ = return ()
printImage (a:rest) i = do
                    if a > 0 then
                        putStr "x"
                    else
                        putStr "|"
                    if i `mod` 28 == 27 then do
                        putStrLn ""
                        return ()
                    else
                        return ()
                    printImage rest (i+1)
                    
