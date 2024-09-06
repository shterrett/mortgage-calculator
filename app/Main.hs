{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Options (Options(..), unwrapRecord)

main :: IO ()
main = do
  o@Options{..} <- unwrapRecord "mortgage-calculator"
  putStrLn $ show o
  pure ()
