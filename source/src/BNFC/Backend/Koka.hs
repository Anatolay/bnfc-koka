module BNFC.Backend.Koka where

import qualified BNFC.Backend.C as C

import BNFC.Backend.Base
import BNFC.CF
import BNFC.Options

import BNFC.Backend.Koka.CFtoKokaAST
import BNFC.Backend.Koka.CFtoParse

makeKoka :: SharedOptions -> CF -> MkFiles ()
makeKoka opts cf = do
  C.makeC opts cf
  mkKokaFile "ast.kk" (mkAstFile opts cf)
  mkKokaFile "parse.kk" kkParseFile
  mkCFile "parse.c" cParseFile
  where
    (kkParseFile, cParseFile) = mkParse opts cf

-- makefile :: Doc
-- makefile = C.makefile undefined undefined undefined

mkCFile :: FilePath -> String -> MkFiles ()
mkCFile path = mkfile path comment

mkKokaFile :: FilePath -> String -> MkFiles ()
mkKokaFile path = mkfile path comment 

-- | Put string into a block comment.
comment :: String -> String
comment x = unwords ["/*", x, "*/"]