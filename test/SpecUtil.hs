module SpecUtil where

import Test.Hspec 
import Test.HUnit.Base (assertFailure)

import Parse
import Lambda.Pretty (PrettyLambda (..))
import Lambda.Reduce (reduce)
import Lambda.Enriched (ToEnriched (..))
import qualified Miranda.Syntax as M (Prog (..), Decl)
import Lambda.ToLambda

import Lambda.Eval 

readMiranda :: FilePath -> IO String
readMiranda base_name = readFile $ "test-ref/" ++ base_name ++ ".m"

readEnriched :: FilePath -> IO String
readEnriched base_name = readFile $ "test-ref/" ++ base_name ++ ".el"

readLambda :: FilePath -> IO String
readLambda base_name = readFile $ "test-ref/" ++ base_name ++ ".l"

ioShouldBe :: (Show a, Eq a) => IO a -> a -> IO ()
ioShouldBe io val = (`shouldBe` val) =<< io

showEvaledMiranda :: String -> IO String 
showEvaledMiranda input = pShow . eval <$> (parseHunit input :: IO M.Prog)

showReducedMiranda :: String -> IO String
showReducedMiranda input = pShow . reduce <$> (parseHunit input :: IO M.Prog)

showLambdadMiranda :: String -> IO String
showLambdadMiranda input = pShow . toLambda <$> (parseHunit input :: IO M.Prog)

showEnrichedMiranda :: String -> IO String 
showEnrichedMiranda input = do parsed_prog <- parseHunit input :: IO M.Prog
                               return $ pShow (toEnriched parsed_prog)

parseProgIO :: String -> IO String
parseProgIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

parseMirandaExpIO :: String -> IO String 
parseMirandaExpIO input = pShow <$> (parseHunit :: String -> IO M.Prog) input

-- parsesDefTo :: String -> String -> IO ()
-- parsesDefTo input res = parseDeclIO input `ioShouldBe` res

parseMatchesProg :: String -> IO ()
parseMatchesProg input = parseProgIO input `ioShouldBe` input

parseMatchesDef :: String -> IO ()
parseMatchesDef input = parseDeclIO input `ioShouldBe` input

parseDeclIO :: String -> IO String
parseDeclIO input = pShow <$> (parseHunit :: String -> IO M.Decl) input 

parseHunit :: Parse a => String -> IO a
parseHunit = eitherHUnit . parse

eitherHUnit :: Either String a -> IO a
eitherHUnit = either assertFailure pure