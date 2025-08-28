module BNFC.Backend.Koka.CFtoKokaAST where


import BNFC.CF
import Data.List (intercalate)
import BNFC.Backend.Common.NamedVariables

mkAstFile :: CF -> String
mkAstFile cf = unlines $ concat
  [ [ "module " ++ moduleName
    , ""
    , "pub struct ident(string : string)"
    , ""
    ]

  , map printData $ filter (not . isList . fst) datas

  , []
  ]
  where
    moduleName = "ast" -- TODO!
    datas :: [Data]
    datas = getAbstractSyntax cf

printData :: Data -> String
printData (cat, rules)
  | isList cat = ""
  | otherwise = unlines $ concat
    [ [ "pub type " ++ firstLowerCase (identCat cat) ]

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


dropList :: String -> (Bool, String)
dropList ('L' : 'i' : 's' : 't' : s) = (True, s)
dropList s = (False, s)

printFieldName :: IVar -> String
printFieldName (s, num)
  | num == 0 = typ
  | otherwise = typ ++ "_" ++ show num
  where
    typ = firstLowerCase typRaw
    (_, typRaw) = dropList s

printFieldType :: String -> String
printFieldType s
  | isList = "list<" ++ typ ++ ">"
  | otherwise = typ
  where
    typ = firstLowerCase typRaw
    (isList, typRaw) = dropList s
