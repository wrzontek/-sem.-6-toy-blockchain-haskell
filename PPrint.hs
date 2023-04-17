-- am418419
module PPrint where

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k,v) = f where f = ((k ++ ": " ++ show v) ++)

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS ("\n" ++)
pprH = intercalateS (" " ++)

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS _ [] = f where f s = ""
intercalateS sep [x] = x
intercalateS sep (h:t) = h . sep . intercalateS sep t

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith f [] = g where g s = ""
pprListWith f [x] = f x
pprListWith f (h:t) = f h . ("\n" ++) . pprListWith f t

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
