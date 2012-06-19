import System.Random
import System.Environment

randomInterval = do 
	minute1 <- randomRIO (0 :: Integer, 59 :: Integer)
	minute2 <- randomRIO (0 :: Integer, 59 :: Integer)
	hour1 <- randomRIO (0 :: Integer, 23 :: Integer)
	hour2 <- randomRIO (0 :: Integer, 23 :: Integer)
	if hour1*60 + minute1 <= hour2*60 + minute2
		then return $ show hour1 ++ ":" ++ show minute1 ++ "," ++ show hour2 ++ ":" ++ show minute2 ++ "\n"
	else
		randomInterval

writeIntervals f n 
	| n == 0 = return ()
	| otherwise = do
		sample <- randomInterval
		appendFile f sample
		writeIntervals f (n-1)

getNumberSamples = do
	args <- getArgs
	case args of
		[a,b] -> return (read a :: Int, b)
		otherwise -> error "Provide number of samples and a filename"

main = do
	(nrSamples, fileName) <- getNumberSamples
	writeIntervals fileName nrSamples
	
	
