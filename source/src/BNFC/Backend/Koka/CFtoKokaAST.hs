module BNFC.Backend.Koka.CFtoKokaAST where


import BNFC.CF
import Data.List (intercalate)
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Koka.Common

mkAstFile :: CF -> String
mkAstFile cf = unlines $ concat
  [ [ "module " ++ moduleName
    , ""
    , "pub struct ident(string : string)"
    , ""
    ]

  , [ "/* User-defined tokens */"]
  , map printToken userTokens

  , [ ""
    , "/* The abstract syntax */"
    , ""
    ]
  , map printData $ filter (not . isList . fst) datas

  ]
  where
    moduleName = "ast" -- TODO!
    userTokens = tokenNames cf
    printToken tk = "pub struct " ++ firstLowerCase tk ++ "(string : string)"
    datas :: [Data]
    datas = getAbstractSyntax cf

printData :: Data -> String
printData (cat, rules)
  | isList cat = ""
  | otherwise = unlines $ concat
    [ [ "pub type " ++ escapeKeywords (firstLowerCase (identCat cat)) ]

    , map printRule rules

    , []
    ]
  where
    printRule (conName, cats) = "  " ++ conName ++ printFields cats

printFields :: [Cat] -> String
printFields cats
  | isEmpty ivars = ""
  | otherwise = " (" ++ intercalate ", " (map printField ivars) ++ ")"
  where
    isEmpty [] = True
    isEmpty _ = False
    ivars = getVars cats


printField :: IVar -> [Char]
printField ("Integer", num)
  = if num == 0 then "int : int" else "int_" ++ show num ++ " : int"
printField (con, num)
  = printFieldName (con, num) ++ " : " ++ printFieldType con


printFieldName :: IVar -> String
printFieldName (s, num)
  | num == 0 = fieldname
  | otherwise = fieldname ++ "_" ++ show num
  where
    fieldname = if isList then fieldnameRaw ++ "List" else fieldnameRaw
    fieldnameRaw = escapeKeywords (firstLowerCase typRaw)
    (isList, typRaw) = dropList s

printFieldType :: String -> String
printFieldType s
  | isList = "list<" ++ typ ++ ">"
  | otherwise = typ
  where
    typ = escapeKeywords (firstLowerCase typRaw)
    (isList, typRaw) = dropList s
