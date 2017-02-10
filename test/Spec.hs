module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Control/Monad/Writer/Stricter.hs"]
