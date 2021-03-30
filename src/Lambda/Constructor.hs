module Lambda.Constructor 
  ( Constructor (..)
  , ConstructorType (..)
  , arity
  , isSum
  , isProduct
  , selectFunctions
  , siblings
  , nTuple
  , fromString
  , unpackStr
  ) where 

import Data.Maybe (fromMaybe)
import GHC.Exts (IsString (..))
import Lambda.Pretty
import qualified Data.Map as Map

data Constructor = Constructor {
  constrName :: String,
  constrType :: ConstructorType
} deriving (Ord, Eq)

data ConstructorType = Sum     Int Int -- tag arity
                     | Product     Int --     arity
                     deriving (Ord, Eq)

instance Show Constructor where 
  show = constrName

instance IsString Constructor where 
  fromString c = Constructor c (fromString c)

-- builtin constructors known statically
instance IsString ConstructorType where 
  fromString "LEAF"   = Sum 1 1
  fromString "BRANCH" = Sum 2 2

  fromString "NIL"    = Sum 1 0
  fromString "CONS"   = Sum 2 2

  fromString "PAIR"   = Product 2
  fromString "TRIPLE" = Product 3
  fromString ('T':'U':'P':'L':'E':'-':ns) = Product (read ns)

  fromString c = error $ "Unknown constructor: " ++ c

instance PrettyLambda Constructor where 
  prettyDoc' n c = prettyDoc' n (constrName c)

--------------------------------------------------------------------------------
-- properties

arity :: Constructor -> Int
arity = arity' . constrType

arity' :: ConstructorType -> Int 
arity' (Sum _ a) = a
arity' (Product a) = a

isSum :: Constructor -> Bool 
isSum = not . isProduct

isProduct :: Constructor -> Bool
isProduct = isProduct' . constrType

isProduct' :: ConstructorType -> Bool
isProduct' (Product _) = True
isProduct' (Sum _ _) = False

selectFunctions :: Constructor -> [String]
selectFunctions = selectFunctions' . arity

selectFunctions' :: Int -> [String]
selectFunctions' arity_val = 
  map (\n -> "SEL-" ++ show arity_val ++ "-" ++ show n) [1..arity_val]


--------------------------------------------------------------------------------
-- builtin

nTuple :: Int -> Constructor
nTuple n 
  | n < 2     = error $ "No tuples for n: " ++ show n
  | n == 2    = fromString "PAIR"
  | n == 3    = fromString "TRIPLE"
  | otherwise = fromString ("TUPLE-" ++ show n)

siblings :: Constructor -> [Constructor]
siblings c = 
  case constrType c of 
    Product _ -> [c]
    Sum _ _   -> 
      fromMaybe (error $ "Sum constructor has no siblings: " ++ show c)
                (Map.lookup (constrName c) builtinSums)

builtinSums :: Map.Map String [Constructor]
builtinSums = foldr foldF Map.empty [biTree, biList]
  where 
    foldF :: [Constructor] 
          -> Map.Map String [Constructor] 
          -> Map.Map String [Constructor]
    foldF cs m = foldr (\c m' -> Map.insert (constrName c) cs m') m cs

biTree :: [Constructor]
biTree =
  [ Constructor "LEAF"   (Sum 1 1),
    Constructor "BRANCH" (Sum 2 2) ]

biList :: [Constructor]
biList =
  [ Constructor "NIL"  (Sum 1 0),
    Constructor "CONS" (Sum 2 2) ]

--------------------------------------------------------------------------------
-- lambda functions

packStr :: Constructor -> String
packStr c = packStr' (constrType c)

packStr' :: ConstructorType -> String
packStr' (Sum tag ar) = "PACK-SUM-" ++ show tag ++ "-" ++ show ar
packStr' (Product ar) = "PACK-PRODUCT-" ++ show ar

unpackStr :: Constructor -> String 
unpackStr c = unpackStr' (constrType c)

unpackStr' :: ConstructorType -> String
unpackStr' (Sum tag ar) = "UNPACK-SUM-" ++ show tag ++ "-" ++ show ar
unpackStr' (Product ar) = "UNPACK-PRODUCT-" ++ show ar