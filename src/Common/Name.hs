module Common.Name (newName) where

import Data.Maybe
import qualified Data.List.NonEmpty as L

newName :: [String] -> String
newName = maybe (numToName 1) numToName . safeMaximum . mapMaybe nameNum

safeMaximum :: [Int] -> Maybe Int
safeMaximum xs = maximum <$> L.nonEmpty xs

numToName :: Int -> String 
numToName x = "_u" ++ show x

nameNum :: String -> Maybe Int
nameNum ('_' : 'u' : num) = Just $ read num
nameNum _ = Nothing