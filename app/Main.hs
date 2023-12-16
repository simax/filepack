module Main where

import qualified FilePack

main :: IO ()
main = do
  let result = FilePack.testRoundTrip FilePack.sampleFilePack
  putStrLn $ "Filepack result: " ++ show result

