module Main where

  import System.Environment (getArgs)
  import Control.Monad (foldM)


  printHelp = putStrLn . unlines $
    ["Usage: macroista input"]


  includeFiles content = foldM includeDirective "" $ lines content where
    includeDirective content' line = do
      let words' = words line
      if null words' 
        then return (content' ++ "\n" ++ line)
        else do
          let firstWord = head words'
          if "@include" /= firstWord
            then return (content' ++ "\n" ++ line)
            else do
              let filename = unwords $ tail words'
              includeFileContent <- readFile filename
              return (content' ++ "\n" ++ includeFileContent)



  processFile filename = readFile filename >>= includeFiles >>= putStrLn


  main :: IO ()
  main = do
    arguments <- getArgs
    if null arguments
      then printHelp
      else processFile (head arguments)
