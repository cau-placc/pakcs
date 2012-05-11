module putModuleHead where

import CurryStringClassifier
import List
import System
import IOExts

main = do args <- getArgs 
          case args of 
           [name] -> readCompleteFile name >>= putHead name
           _ -> putStrLn "usage: imports filename"

isModuleDecl x = isPrefixOf "module " x

maybeModHead name conts hs 
  | length hs == 1 = putStrLn $ name ++ " already has module declaration" 
  | length hs == 0 = writeFile name ("module "++woCurry name++" where\n\n"++conts) >>
                     putStrLn "module declaration added"
  | otherwise = error "more than one mdule declaration?"

putHead name conts = maybeModHead name conts $
                  (filter isModuleDecl
                  . lines
                  . unscan 
                  . tokTakeWhile isModuleHead 
                  . tokFilter (not . isComment) 
                  . scan) conts

woCurry s = case reverse s of
             'y':'r':'r':'u':'c':'.':wo -> reverse wo
             _ -> error "not a curry file" 