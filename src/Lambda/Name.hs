module Lambda.Name (newName, nextName, nextNames) where 

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (isLower)

-- | Identifies a new name, starting with 
-- | taken names and a desired name.
-- | If the desired name is taken, then iterates 
-- | through possible names until it's found one not taken
newName :: [String] -> String
newName taken = if "a" `elem` taken then nextName' (Set.fromList taken) "a" else "a"

nextName :: [String] -> String -> String 
nextName taken = nextName' (Set.fromList taken)

nextNames :: [String] -> String -> [String]
nextNames taken = iterate (nextName taken)

nextName' :: Set String -> String -> String
nextName' taken name
  | succ_name `Set.member` taken = nextName' taken succ_name
  | otherwise = succ_name
  where 
    succ_name = succName name

succName :: String -> String 
succName [] = error "empty name"
succName [c] 
  | not (isLower c) = error $ "variable not lowercase: " ++ [c]
  | c == 'z'        = "z'"
  | otherwise       = [succ c]
succName name = name ++ "'"