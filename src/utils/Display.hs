{-# LANGUAGE OverloadedStrings #-}
module Utils.Display (
    bold,
    colorizeSeverity
) where

import qualified Data.Text as T

-- | Helper to bold text using ANSI codes
bold :: String -> String
bold s = "\ESC[1m" ++ s ++ "\ESC[0m"

-- | Helper to colorize severity
colorizeSeverity :: T.Text -> String -> String
colorizeSeverity sevText s
    | sev == "Good" = "\ESC[32m" ++ s ++ "\ESC[0m" -- Green
    | sev == "Serious" || sev == "Severe" = "\ESC[31m" ++ s ++ "\ESC[0m" -- Red
    | otherwise = "\ESC[33m" ++ s ++ "\ESC[0m" -- Yellow (Warning/Other)
  where sev = T.unpack sevText
