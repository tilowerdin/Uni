diamond :: Int -> IO()
diamond 0 = putStrLn ""
diamond k = diamond1 0 k
  where 
    diamond1 spaces 1 = putStrLn (takeSpaces spaces ++ "*")
    diamond1 spaces n = 
        do diamond2 (spaces+1) (n-1)
           putStrLn (takeSpaces spaces ++ "*" ++ takeSpaces ((n-1)*2 - 1) ++ "*")
           diamond3 (spaces+1) (n-1)

    diamond2 spaces 1 = diamond1 spaces 1
    diamond2 spaces n = 
        do diamond2 (spaces+1) (n-1)
           putStrLn (takeSpaces spaces ++ "*" ++ takeSpaces ((n-1)*2 - 1) ++ "*")
    
    diamond3 spaces 1 = diamond1 spaces 1
    diamond3 spaces n = 
        do putStrLn (takeSpaces spaces ++ "*" ++ takeSpaces ((n-1)*2 - 1) ++ "*")
           diamond3 (spaces+1) (n-1)

    takeSpaces n = take n $ repeat ' '
                             
tree :: Int -> IO()
tree 0 = putStrLn ""
tree k = diamond1 0 k
  where 
    diamond1 spaces 1 = putStrLn (takeSpaces spaces ++ "*")
    diamond1 spaces n = 
        do diamond2 (spaces+1) (n-1)
           putStrLn (takeSpaces spaces ++ takeDiamonds (n*2 - 1))

    diamond2 spaces 1 = diamond1 spaces 1
    diamond2 spaces n = 
        do diamond2 (spaces+1) (n-1)
           putStrLn (takeSpaces spaces ++ takeDiamonds (n*2 - 1))

    takeSpaces n = take n $ repeat ' '

    takeDiamonds n = take n $ repeat '*'
