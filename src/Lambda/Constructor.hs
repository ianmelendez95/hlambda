module Lambda.Constructor 
  ( Constructor (..)
  , ConstructorType (..)
  , Sum (..)
  , Product (..)

  , structTag

  , arity
  , arity'

  , isSum
  , isProduct

  , selectFunction
  , selectFunctions
  , selectFunctions'

  , siblings
  , nProduct
  , tupleForProduct
  , nTuple
  , fromString
  , unpackStr
  ) where 

import Data.Maybe (fromMaybe)
import GHC.Exts (IsString (..))
import Lambda.Pretty ( PrettyLambda(prettyDoc') )
import qualified Data.Map as Map

data Constructor = Constructor {
  constrName :: String,
  constrType :: ConstructorType
} deriving (Ord, Eq)

data ConstructorType = Sum Sum        
                     | Product Product
                     deriving (Ord, Eq)

data Sum = CSum Int Int      -- tag arity
         deriving (Ord, Eq)
newtype Product = CProduct Int  -- arity
                deriving (Ord, Eq)

--------------------------------------------------------------------------------
-- String: show, fromString

instance Show Constructor where 
  show = constrName

instance IsString Constructor where 
  fromString c = Constructor c (fromString c)

-- builtin constructors known statically
instance IsString ConstructorType where 
  fromString "LEAF"   = mkSumType 1 1
  fromString "BRANCH" = mkSumType 2 2

  fromString "NIL"    = mkSumType 1 0
  fromString "CONS"   = mkSumType 2 2

  fromString "UNIT"   = mkProductType 0
  fromString "SINGLE" = mkProductType 1
  fromString "PAIR"   = mkProductType 2
  fromString "TRIPLE" = mkProductType 3
  fromString ('T':'U':'P':'L':'E':'-':ns) = mkProductType (read ns)

  fromString c = error $ "Unknown constructor: " ++ c

instance PrettyLambda Constructor where 
  prettyDoc' n c = prettyDoc' n (constrName c)

--------------------------------------------------------------------------------
-- constructors

mkSumType :: Int -> Int -> ConstructorType
mkSumType tag arty = Sum $ CSum tag arty

mkProductType :: Int -> ConstructorType
mkProductType = Product . CProduct

tupleForProduct :: Product -> Constructor
tupleForProduct (CProduct arty) = nProduct arty

--------------------------------------------------------------------------------
-- properties

structTag :: Constructor -> Int
structTag Constructor { constrType = (Product _) } = 0
structTag Constructor { constrType = (Sum (CSum t _)) } = t

arity :: Constructor -> Int
arity = arity' . constrType

arity' :: ConstructorType -> Int 
arity' (Sum (CSum _ a)) = a
arity' (Product (CProduct a)) = a

isSum :: Constructor -> Bool 
isSum = not . isProduct

isProduct :: Constructor -> Bool
isProduct = isProduct' . constrType

isProduct' :: ConstructorType -> Bool
isProduct' (Product _) = True
isProduct' (Sum _) = False

selectFunction :: Int -> Constructor -> String
selectFunction n c = 
  let arty = arity c
   in if n > arty 
        then error $ "Constructor does not have a select function for argument " ++ show n ++ ": " ++ show c
        else "SEL-" ++ show (arity c) ++ "-" ++ show n

selectFunctions :: Constructor -> [String]
selectFunctions = selectFunctions' . arity

selectFunctions' :: Int -> [String]
selectFunctions' arity_val = 
  map (\n -> "SEL-" ++ show arity_val ++ "-" ++ show n) [1..arity_val]


--------------------------------------------------------------------------------
-- builtin

nProduct :: Int -> Constructor
nProduct n 
  | n < 0 = error $ "Products cannot have negative arity: " ++ show n
  | n == 0 = fromString "UNIT"
  | n == 1 = fromString "SINGLE"
  | otherwise = nTuple n

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
    Sum _   -> 
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
  [ fromString "LEAF",
    fromString "BRANCH" ]

biList :: [Constructor]
biList =
  [ fromString "NIL",
    fromString "CONS" ]

--------------------------------------------------------------------------------
-- lambda functions

packStr :: Constructor -> String
packStr c = packStr' (constrType c)

packStr' :: ConstructorType -> String
packStr' (Sum (CSum tag ar)) = "PACK-SUM-" ++ show tag ++ "-" ++ show ar
packStr' (Product (CProduct ar)) = "PACK-PRODUCT-" ++ show ar

unpackStr :: Constructor -> String 
unpackStr c = unpackStr' (constrType c)

unpackStr' :: ConstructorType -> String
unpackStr' (Sum (CSum tag ar)) = "UNPACK-SUM-" ++ show tag ++ "-" ++ show ar
unpackStr' (Product (CProduct ar)) = "UNPACK-PRODUCT-" ++ show ar