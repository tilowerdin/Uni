import Data.List

menu :: [String] -> IO Int
menu list = do putStrLn "Wählen Sie eines der folgenden Menüelemente: "
               putStr (unlines list)
               input <- getLine
               putStrLn ""
               case elemIndex input list of
                 Nothing -> do putStrLn ("Element " ++ show input ++ " nicht im Menü enthalten.")
                               menu list
                 Just index -> return index 

wordCount :: String -> IO ()
wordCount file = do fileString <- readFile file
                    putStrLn "1: count chars, 2: count words, 3: count lines, 4: quit"
                    pressed <- getLine
                    case pressed of 
                      "1" -> do putStrLn $ (show $ length fileString) ++ " chars"
                                wordCount file
                      "2" -> do putStrLn $ (show $ length $ words fileString) ++ " words"
                                wordCount file
                      "3" -> do putStrLn $ (show $ length $ lines fileString) ++ " lines"
                                wordCount file
                      "4" -> putStrLn "bye"
                      otherwise -> do putStrLn "you should type in a number between 1 and 4"
                                      wordCount file

onSelect :: [(String, IO a)] -> IO a
onSelect list = do index <- menu (map fst list)
                   snd $ list !! index