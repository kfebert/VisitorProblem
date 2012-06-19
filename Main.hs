import System.Environment

import Visitor

retrieveArgument = do 
       args <- getArgs
       case args of
                [f] -> return f
                otherwise -> printUsage
main::IO()
main = do
       filename <- retrieveArgument
       file <- readFile filename
       printTuple $ determineMaxVisitingTimes file
       
printUsage = error "Usage: file.txt" 
