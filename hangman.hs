main :: IO ()
main = do
    file <- readFile "words"
    let wordsOfGame = lines file
    game wordsOfGame

changeLetters :: Char -> Int -> String -> String
changeLetters letter pos final = take pos final ++ [letter] ++ drop (pos + 1) final

replace :: Char -> Int -> String -> String -> String
replace _ _ [] final = final
replace letter pos (w : word) final = 
    if letter == w 
    then replace letter (pos + 1) word (changeLetters letter pos final) 
    else replace letter (pos + 1) word final

rightWord :: String ->  String -> String -> String
rightWord _ "" xs = xs
rightWord word (letter : guess) xs = 
    if elem letter word 
    then rightWord word guess (replace letter 0 word xs)
    else rightWord word guess xs

writelline :: Int -> String
writelline 0 = ""
writelline n = "-" ++ writelline (n-1)

game :: [String] -> IO()
game [] = return ()
game (word : wordsOfGame)  = do
    putStrLn  "I'm thinking of a word:"
    putStrLn  (writelline (length word))
    gamePlan word
    putStrLn "Play again?"
    answer <- getLine 
    if answer == "yes" 
    then game wordsOfGame 
    else return()

gamePlan :: String -> IO()
gamePlan word = do
    putStrLn "Your guess?"
    guess <- getLine 
    if length word /= length guess 
        then  putStrLn "Wrong number of letters" >> gamePlan word
    else if word /= guess 
        then putStrLn (rightWord word guess (writelline (length word))) >> gamePlan word
    else putStrLn "Correct!"
