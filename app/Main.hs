module Main where

import Lib
import qualified System.Environment as Env

main :: IO ()
main = do
     args <- Env.getArgs
     p <- mapM parseFile args
     mapM_ myPrint p

parseFile file = do
        input <- readFile file
        return $ snd <$> runParser jsonValue input


myPrint (Just input) = print input
myPrint Nothing = mempty
