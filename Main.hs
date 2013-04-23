module Main where

  import System.Environment (getArgs)
  import Control.Monad (foldM)


  printHelp = putStrLn . unlines $
    ["Usage: macroista input"]


  safeHead :: [a] -> Maybe a
  safeHead []    = Nothing
  safeHead (x:_) = Just x


  includeFiles content = foldM includeDirective "" $ lines content where
    includeDirective content' line = do
      let filename = let words' = words line in safeHead words' >>= (\firstWord -> if firstWord == "@include" then Just (unwords $ tail words') else Nothing)
      case filename of
          Nothing -> return (content' ++ "\n" ++ line)
          Just f  -> readFile f >>= (\fc -> return (content' ++ "\n" ++ fc))


  processFile filename = readFile filename >>= includeFiles >>= putStrLn


  main :: IO ()
  main = do
    arguments <- getArgs
    if null arguments
      then printHelp
      else processFile (head arguments)
