-- | Primarily for use in GHCI
module GhciUtil where 

import Parse
import Lambda.Reduce (Reducible (..))
import Lambda.Pretty (PrettyLambda (..))
import Lambda.ToLambda
import Lambda.Enriched (ToEnriched (..))

import Miranda.Lexer (scanTokens)
import Miranda.Syntax (Prog, Decl)
import qualified Miranda.Syntax as M

evalMiranda :: String -> IO () 
evalMiranda = pPrint . reduce . either error id . (parse :: String -> Either String Prog)

toLambdaMiranda :: String -> IO () 
toLambdaMiranda = pPrint . toLambda . parseMiranda

enrichMiranda :: String -> IO ()
enrichMiranda = pPrint . toEnriched . parseMiranda

parseMiranda :: String -> Prog
parseMiranda = eitherError . parse

parseMirandaDecl :: String -> Decl 
parseMirandaDecl = eitherError . parse

parseMirandaExp :: String -> M.Exp
parseMirandaExp = eitherError . parse

lexMiranda :: String -> String
lexMiranda = show . scanTokens

pPrint :: PrettyLambda a => a -> IO ()
pPrint = putStrLn . pShow

eitherError :: Either String a -> a
eitherError = either error id

bookToPdfPage :: Int -> Int
bookToPdfPage book_page = book_page + 13

-- tree ::= Leaf num | Branch tree tree