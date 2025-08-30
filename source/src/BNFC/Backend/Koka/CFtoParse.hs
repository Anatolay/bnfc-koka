module BNFC.Backend.Koka.CFtoParse where


import Data.List (nub, intercalate)

import BNFC.CF
import BNFC.Utils
import BNFC.Backend.Common.NamedVariables
import BNFC.Backend.Koka.Common (escapeKeywords, escapeKeywordsDouble)
import BNFC.Options


-- | Generate parser files: .kk and .c
mkParse :: SharedOptions -> CF -> (String, String)
mkParse opts cf = (mkKokaParse opts cf, mkFfiParse opts cf)

mkKokaParse :: SharedOptions -> CF -> String
mkKokaParse opts cf = unlines $ concat
  [ [ "module " ++ moduleName
    , ""
    , "import " ++ syntaxModuleName
    , ""
    , "extern import"
    , "  c file \"parse.c\""
    , ""
    ]

    , concatMap handleEntrypoint (allEntryPoints cf)

    , []
  ]
  where
    package = maybe "" (++ "/") (inPackage opts)
    moduleName = package ++ "parse"
    syntaxModuleName = package ++ "ast"
    handleEntrypoint :: Cat -> [String]
    handleEntrypoint cat =
      [ "pub extern parse" ++ funName ++ "(str : string) : maybe<" ++ returnType ++ ">"
      , "  c \"kk_parse_" ++ identCat cat ++ "\""
      , ""
      ]
      where
        (isList, typ) = dropList (identCat cat)
        kokaType = escapeKeywords . firstLowerCase . identCat . normCatOfList $ cat
        returnType = if isList then "list<" ++ kokaType ++ ">" else kokaType
        funName = if isList then typ ++ "List" else typ


mkFfiParse :: SharedOptions -> CF -> String
mkFfiParse opts cf = unlines $ concat
  [ [ "#include \"Parser.h\""
    , "#include \"Printer.h\""
    , "#include \"Absyn.h\""
    , ""
    ]

  -- conversion between C and Koka strings
  , [ "/* *********** Conversion between Koka and C string **********  */"
    , "uint8_t* string_to_chars(kk_string_t str, kk_context_t* _ctx) {"
    , "  kk_ssize_t len;"
    , "  const uint8_t* cstr = kk_string_buf_borrow(str, &len, _ctx);"
    , "  return cstr;"
    , "}"
    , "kk_string_t chars_to_string(char* str, kk_context_t* _ctx) {"
    , "  kk_string_t kstr = kk_string_alloc_from_utf8(str, _ctx);"
    , "  return kstr;"
    , "}"
    , ""
    ]

  , [ "/* ************** Handle reserved BNFC types ************ */" ]

  -- Convert C `Ident` into Koka `ident`
  , [ kkIdentType ++ " convert_Ident(Ident ident, kk_context_t* _ctx) {"
    , "  kk_string_t str = chars_to_string(ident, _ctx);"
    , "  return " ++ kkIdentCon ++ "(str, _ctx);"
    , "}"
    ]

  -- Convert C `Integer` into Koka `int`
  , [ "kk_integer_t convert_Integer(Integer i, kk_context_t* _ctx) {"
    , "  return kk_integer_from_int(i, _ctx);"
    , "}"
    , ""
    ]

  , [ "/* ************** Declarations ************ */" ]
  -- Declare conversion functions for user-defined tokens
  , map printToken userTokens
  -- Declare other conversion functions
  , map (printDecl prefix) classes
  , [ "", "" ]

  -- Generate all conversion functions
  , [ "/* ************** Convert C AST into Koka AST ************ */" ]
  , concatMap convertToken userTokens
  , map (printConversionFun prefix) datas

  -- Handle entrypoints
  , [ "/* ************** Entrypoints (functions to call from Koka) ************ */" ]
  , concatMap handleEntrypoint (allEntryPoints cf)
  ]
  where
    path = maybe "ast" (++ "/ast") (inPackage opts)
    prefix = intercalate "_" ("kk" : path `splitBy` '/') ++ "__"
    kkIdentType = prefix ++ "ident"
    kkIdentCon = prefix ++ "new_Ident"
    userTokens = tokenNames cf
    printToken tk = prefix ++ firstLowerCase tk ++ " convert_" ++ tk ++ "(" ++ tk +++ firstLowerCase tk ++ ", kk_context_t* _ctx);"
    convertToken tk =
      [ prefix ++ firstLowerCase tk ++ " convert_" ++ tk ++ "(" ++ tk +++ firstLowerCase tk ++ ", kk_context_t* _ctx) {"
      , "  kk_string_t str = chars_to_string(" ++ firstLowerCase tk ++", _ctx);"
      , "  return " ++ prefix ++ "new_" ++ tk ++ "(str, _ctx);"
      , "}"
      ]
    datas :: [Data]
    datas = getAbstractSyntax cf
    classes :: [String]
    classes = nub $ map (identCat . fst) datas
    handleEntrypoint :: Cat -> [String]
    handleEntrypoint cat =
      [ "kk_std_core_types__maybe kk_parse_" ++ identCat cat ++ "(kk_string_t kstr, kk_context_t* _ctx) {"
      , "  const uint8_t* cstr = string_to_chars(kstr, _ctx);"
      , "  " ++ identCat (normCat cat) ++ " parse_tree;"
      , "  parse_tree = ps" ++ identCat cat ++ "(cstr);"
      , "  if (parse_tree) {"
      , "    " ++ returnType ++ " converted = convert_" ++ identCat (normCat cat) ++ "(parse_tree, _ctx);"
      , "    return kk_std_core_types__new_Just(" ++ boxFun ++ "(converted, _ctx), _ctx);"
      , "  }"
      , "  else {"
      , "    return kk_std_core_types__new_Nothing(_ctx);"
      , "  }"
      , "}"
      ]
      where
        (isList, typ) = dropList (identCat (normCat cat))
        kokaType = escapeKeywordsDouble (firstLowerCase typ)
        returnType = if isList then "kk_std_core_types__list" else prefix ++ kokaType
        boxFun = if isList then "kk_std_core_types__list_box" else prefix ++ kokaType ++ "_box"

mkSignature :: String -> String -> String
mkSignature prefix cType = returnType +++ "convert_" ++ cType ++ "(" ++ cType +++ firstLowerCase cType ++ ", kk_context_t* _ctx)"
  where
    (isList, typ) = dropList cType
    kokaType = escapeKeywordsDouble (firstLowerCase typ)
    returnType = if isList then "kk_std_core_types__list" else prefix ++ kokaType

printDecl :: String -> String -> String
printDecl prefix cType = mkSignature prefix cType ++ ";"

printConversionFun :: String -> Data -> String
printConversionFun prefix (cat, rules)
  | isList cat = unlines
    [ mkSignature prefix cType ++ " {"
    , "  if (" ++ cVar ++ ") {"
    , "    " ++ wrappedType +++ wrappedVaR ++ " = " ++ cVar ++ "->" ++ varName wrappedType ++ ";"
    , "    " ++ prefix ++ escapeKeywordsDouble wrappedVaR ++ " k1 = " ++ "convert_" ++ wrappedType ++ "(" ++ wrappedVaR ++ ", _ctx);"
    , "    kk_box_t k2 = " ++ prefix ++ escapeKeywordsDouble wrappedVaR ++ "_box(k1, _ctx);"
    , "    " ++ cVar ++ " = " ++ cVar ++ "->" ++ varName cType ++ ";"
    , "    kk_std_core_types__list k3 = convert_" ++ cType ++"(" ++ cVar ++ ", _ctx);"
    , "    return kk_std_core_types__new_Cons(kk_reuse_null, 0, k2, k3, _ctx);"
    , "  }"
    , "  else {"
    , "    return kk_std_core_types__new_Nil(_ctx);"
    , "  }"
    , "}"
    ]
  | otherwise = unlines
    [ mkSignature prefix cType ++ " {"
    , unlines $ printRules rules
    , "}"
    ]
  where
    -- Type of the AST node in C.
    -- It happens to be the same as the
    -- corresponding Koka constructor name.
    cType = identCat (normCat cat)
    -- A variable name to hold value of the `cType` type.
    -- It happens to be the same as the
    -- corresponding Koka type.
    cVar = firstLowerCase cType

    -- If `cat` is a list category [Ca], get type Ca.
    -- Otherwise the same as `cType` and `cVar`
    wrappedType = identCat (normCatOfList cat)
    wrappedVaR = firstLowerCase wrappedType

    -- TODO: handle case like
    -- SingleLabel. Node ::= WrappedNode
    printRules [(_con, [_cat])] =
      []
    printRules rules =
      [ "  switch(" ++ cVar ++ "->kind) {"
      , unlines $ concatMap printCase (zip rules [0..])
      , "  }"
      ]
    printCase :: ((String, [Cat]), Int) -> [String]
    printCase ((con, []), _) =
      [ "    case is_" ++ con ++ ":"
      , "      return " ++ prefix ++ "new_" ++ con ++ "(_ctx);"
      ]
    printCase ((con, cats), caseNum) =
      [ "    case is_" ++ con ++ ":"
      , unlines $ concatMap handleArg (zip (getVars cats) [1..])
      , "      return " ++ prefix ++ "new_" ++ con ++ "(kk_reuse_null, 0, " ++ printArgs ++ ", _ctx);"
      ]
      where
        printArgs = intercalate ", " [kokaVarPrefix ++ show i | i <- [1..length cats]]
        cVarPrefix = "c" ++ show caseNum ++ "_"
        kokaVarPrefix = "k" ++ show caseNum ++ "_"
        handleArg ((cArgType, occurences), n :: Int) =
          [ replicate 6 ' ' ++ cArgType +++ cn ++ " = " ++ cVar ++ "->u." ++ firstLowerCase con ++ "_." ++ varName cArgType ++ showNum occurences ++ ";"
          , replicate 6 ' ' ++ kokaArgType +++ kn ++ " = convert_" ++ cArgType ++ "(" ++ cn ++ ", _ctx);"
          ]
          where
            cn = cVarPrefix ++ show n
            kn = kokaVarPrefix ++ show n
            (isList, _) = dropList cArgType
            isInteger = case cArgType of
              "Integer" -> True
              _ -> False
            kokaArgType
              | isList = "kk_std_core_types__list"
              | isInteger = "kk_integer_t"
              | otherwise = prefix ++ escapeKeywordsDouble (firstLowerCase cArgType)

dropList :: String -> (Bool, String)
dropList ('L' : 'i' : 's' : 't' : s) = (True, s)
dropList s = (False, s)

splitBy :: String -> Char -> [String]
splitBy str delim = go [] str
  where
    ap x [] = [[x]]
    ap x (xs:xss) = (x:xs) : xss

    go acc [] = reverse $ fmap reverse acc
    go acc (c:str)
      | c == delim = go ([]:acc) str
      | otherwise = go (ap c acc) str
