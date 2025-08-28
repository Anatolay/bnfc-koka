module BNFC.Backend.Koka.Common where

dropList :: String -> (Bool, String)
dropList ('L' : 'i' : 's' : 't' : s) = (True, s)
dropList s = (False, s)

keywords :: [String]
keywords =
  [ "infix" , "infixr" , "infixl"
  , "module" , "import" , "as"
  , "pub" , "abstract"
  , "type" , "struct" , "alias" , "effect" , "con"
  , "forall" , "exists" , "some"
  , "fun" , "fn" , "val" , "var" , "extern"
  , "if" , "then" , "else" , "elif"
  , "match" , "return" , "with" , "in"
  , "handle" , "handler" , "mask"
  , "ctl" , "final" , "raw"
  , "override" , "named"
  , "ctx"
  , "import"
  , "interface" , "break" , "continue" , "unsafe"
  ]

escapeKeywords :: String -> String
escapeKeywords s =
  if s `elem` keywords
  then escapeKeywords (s ++ "_")
  else s

escapeKeywordsDouble :: String -> String
escapeKeywordsDouble s =
  if s `elem` keywords
  then escapeKeywords (s ++ "__")
  else s