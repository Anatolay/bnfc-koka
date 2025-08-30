module BNFC.Backend.Koka where

import qualified BNFC.Backend.C as C

import Prelude hiding ((<>))

import BNFC.Backend.Base
import BNFC.CF
import BNFC.Options
import BNFC.PrettyPrint
import BNFC.Utils

import BNFC.Backend.Koka.CFtoKokaAST
import BNFC.Backend.Koka.CFtoParse

import qualified BNFC.Backend.Common.Makefile as Makefile


makeKoka :: SharedOptions -> CF -> MkFiles ()
makeKoka opts cf = do
  C.makeC opts cf
  mkKokaFile "ast.kk" (mkAstFile opts cf)
  mkKokaFile "parse.kk" kkParseFile
  mkCFile "parse.c" cParseFile
  Makefile.mkMakefile (optMake opts) $ makefile name prefix
  where
    (kkParseFile, cParseFile) = mkParse opts cf

    -- Copied from the C backend
    name :: String
    name = lang opts
    prefix :: String
    prefix = snakeCase_ name ++ "_"

-- makefile :: Doc
-- makefile = C.makefile undefined undefined undefined

mkCFile :: FilePath -> String -> MkFiles ()
mkCFile path = mkfile path comment

mkKokaFile :: FilePath -> String -> MkFiles ()
mkKokaFile path = mkfile path comment 

-- | Put string into a block comment.
comment :: String -> String
comment x = unwords ["/*", x, "*/"]


-- Copied from the C backend
makefile :: String -> String -> String -> Doc
makefile name prefix basename = vcat
    [ "CC = gcc -g"
    , "CCFLAGS = --ansi -W -Wall -Wsign-conversion -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration ${CC_OPTS}"
    -- The @#define _POSIX_C_SOURCE 200809L@ is now placed locally in
    -- the generated lexer.
    -- , "CCFLAGS = --ansi -W -Wall -Wno-unused-parameter -Wno-unused-function -Wno-unneeded-internal-declaration -D_POSIX_C_SOURCE=200809L ${CC_OPTS}"
    -- , "# Setting _POSIX_C_SOURCE to 200809L activates strdup in string.h."
    -- , "# strdup was not in the ISO C standard before 6/2019 (C2x), yet in POSIX 1003.1."
    -- , "# See https://en.cppreference.com/w/c/experimental/dynamic/strdup"
    , ""
    , "FLEX = flex"
    , "FLEX_OPTS = -P" <> text prefix
    , ""
    , "BISON = bison"
    , "BISON_OPTS = -t -p" <> text prefix
    , ""
    , "OBJS = Absyn.o Buffer.o Lexer.o Parser.o Printer.o"
    , ""
    , Makefile.mkRule ".PHONY" ["clean", "distclean"]
      []
    , Makefile.mkRule "all" [testName]
      []
    , Makefile.mkRule "clean" []
      -- peteg: don't nuke what we generated - move that to the "vclean" target.
      [ "rm -f *.o " ++ testName ++ " " ++ unwords
        [ name ++ e | e <- [".aux", ".log", ".pdf",".dvi", ".ps", ""]] ]
    , Makefile.mkRule "distclean" ["clean"]
      [ "rm -f " ++ unwords
        [ "Absyn.h", "Absyn.c"
        , "Bison.h"
        , "Buffer.h", "Buffer.c"
        , name ++ ".l", "Lexer.c"
        , name ++ ".y", "Parser.h", "Parser.c"
        , "Printer.c", "Printer.h"
        , "Skeleton.c", "Skeleton.h"
        , "Test.c"
        , basename, name ++ ".tex"
        ]
      ]
    , Makefile.mkRule testName ["${OBJS}", "Test.o"]
      [ "@echo \"Linking " ++ testName ++ "...\""
      , "${CC} ${OBJS} Test.o -o " ++ testName ]
    , Makefile.mkRule "Absyn.o" [ "Absyn.c", "Absyn.h"]
      [ "${CC} ${CCFLAGS} -c Absyn.c" ]
    , Makefile.mkRule "Buffer.o" [ "Buffer.c", "Buffer.h"]
      [ "${CC} ${CCFLAGS} -c Buffer.c" ]
    , Makefile.mkRule "Lexer.c" [ name ++ ".l" ]
      [ "${FLEX} ${FLEX_OPTS} -oLexer.c " ++ name ++ ".l" ]
    , Makefile.mkRule "Parser.c Bison.h" [ name ++ ".y" ]
      [ "${BISON} ${BISON_OPTS} " ++ name ++ ".y -o Parser.c" ]
    , Makefile.mkRule "Lexer.o" [ "CCFLAGS+=-Wno-sign-conversion" ]
        []
    , Makefile.mkRule "Lexer.o" [ "Lexer.c", "Bison.h" ]
      [ "${CC} ${CCFLAGS} -c Lexer.c " ]
    , Makefile.mkRule "Parser.o" ["Parser.c", "Absyn.h", "Bison.h" ]
      [ "${CC} ${CCFLAGS} -c Parser.c" ]
    , Makefile.mkRule "Printer.o" [ "Printer.c", "Printer.h", "Absyn.h" ]
      [ "${CC} ${CCFLAGS} -c Printer.c" ]
    , Makefile.mkRule "Test.o" [ "Test.c", "Parser.h", "Printer.h", "Absyn.h" ]
      [ "${CC} ${CCFLAGS} -c Test.c" ]
    ]
  where testName = "Test" ++ name