module Visitor (printTuple, determineMaxVisitingTimes) where

import qualified Data.Time
import Data.DateTime
import Data.List
import Data.List.Utils
import Data.Maybe

-- a Visitor consistens of an event time, kind of event (entering or exiting) and who followed him.
data Visitor = None | Visitor {time :: DateTime, kind :: Integer, next :: Visitor} deriving (Eq, Ord)
type Tuple = (Visitor, Integer)

-- Display visitor and next one: %H:%M-%H:%M
instance Show Visitor where 
        show v1 = showTime v1 ++ "-" ++ showTime (next v1) where
                showTime v = formatDateTime "%H:%M" $ time v

printTuple :: [Tuple] -> IO ()
printTuple tup = putStrLn $ concatMap (\x -> show (fst x) ++ "," ++ show (snd x) ++ "\n") tup

-- Order by visitor, prefer entering before exiting ones                               
orderByVisitor :: Visitor -> Visitor -> Ordering
orderByVisitor v1 v2 = if compare (time v1) (time v2) /= EQ then compare (time v1) (time v2)  else compare (kind v2) (kind v1)

-- Order by total sum of visitor, decreasing 
orderBySumOfVisitors :: (Visitor, Integer) -> (Visitor, Integer) -> Ordering
orderBySumOfVisitors t1 t2 = compare (snd t2) (snd t1)  

-- Construct from time HH:MM visitors
buildVisitor :: [String] -> [Visitor]
buildVisitor [i1, i2] = [Visitor entering 1 None, Visitor exiting (-1) None] where
	entering = getTime i1
        exiting = getTime i2
	-- Helper to parse datetimes                        
	getTime x | isJust parsedTime = fromJust parsedTime 
	          | otherwise = error "Invalid data." where
		parsedTime = parseDateTime "%-H:%-M" x
buildVisitor _ = error "Invalid data."


-- Parses file content to a list of visitors and sorts them by datetime.
visitingTimes :: String -> [Visitor]
visitingTimes s = sortBy orderByVisitor $ concatMap (buildVisitor . splitComma) (lines s) where
                splitComma = split ","

-- Computes for each visitor current amount of visitors in the building.
sumOfVisitors :: [Visitor] -> [Tuple]
sumOfVisitors = sumOfVisitors' 0 where
         sumOfVisitors' :: Integer -> [Visitor] -> [Tuple]
         sumOfVisitors' n [x] = [(x, n + kind x)]
         sumOfVisitors' n (x:xs) = (Visitor (time x) (kind x) (head xs), n') : sumOfVisitors' n' xs where
                n' = n + kind x

-- Determines all maxima (expects sorted list by number of visitors)
getMaxima :: [Tuple] -> [Tuple]
getMaxima vs = filter (\x -> firstMax == snd x) vs where
               firstMax = snd $ head vs

-- Takes normally the content of a file with specificed format and returns maximum number of visitors with datetime
determineMaxVisitingTimes :: String -> [Tuple]
determineMaxVisitingTimes s = getMaxima $ sortBy orderBySumOfVisitors $ sumOfVisitors $ visitingTimes s
