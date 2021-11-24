main = do 
        puStrLn "Hello, what's your name"
        name <- getLine
        puStrLn ("Hey " ++ name ++ ", you rock")