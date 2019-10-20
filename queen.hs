-- import System.Environment   
-- import Data.List  

-- main = do  
--     args <- getArgs                  -- IO [String]
--     progName <- getProgName          -- IO String
--     putStrLn "The arguments are:"  
--     mapM putStrLn args  
--     putStrLn "The program name is:"  
--     putStrLn progName
     
solutions n m | n< 1 =  error "invalid queen number"
              | n==1 =  map(:[]) (line 1)
              | otherwise = 
                [q:qs | qs<- solutions (n-1) m, 
                        q<- line n, 
                        and(no_attack q qs)]
                where line n = [ [q,n] | q<-[1..m]]      
                  

    

no_attack q qs =  [helper q b |  b <-  qs]
        where helper a b =  a !! 0 /= b !! 0 
                            && abs( a!!0 - b!!0) /= a!!1-b!!1   

-- main = putStrLn $ show $ length $ (solutions 0 0)


main = putStrLn $ show $ head $ solutions 30 30
