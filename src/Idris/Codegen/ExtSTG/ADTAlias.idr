||| Information about aliasing Haskell defined datatypes with Idris one.
module Idris.Codegen.ExtSTG.ADTAlias

import Core.TT
import Idris.Codegen.ExtSTG.ExtName
import Text.Lexer
import Text.Parser
import Data.List
import Data.SortedMap
import Data.SortedMap.Dependent
import Core.Name
import Core.Context
import System.File

--
-- Read ADT definition from the file as in ForeignFiles. The filename should be Module\Path\Last.adtmap
-- This is a bit confusing as namespaces can be found within modules in idris, but here for the sake of
-- simplicity we use the fully qualified name and as if it were a full path.
--
-- The format of the file:
-- -- comment
-- TypeName = package Module.Path name
-- -- comment
-- | ConstructorName = name arity
-- -- comment
-- | ...
-- | ConstructorName = name arity
--

public export
Arity : Type
Arity = Nat

-- Parsing

data AATokenKind
  = TName
  | TEq
  | TDot
  | TPipe
  | TArity
  | TComment
  | TIgnore

Show AATokenKind where
  show TName = "TName"
  show TEq = "TEq"
  show TDot = "TDot"
  show TPipe = "TPipe"
  show TArity = "TArity"
  show TComment = "TComment"
  show TIgnore = "TIgnore"

Eq AATokenKind where
  TName     == TName    = True
  TEq       == TEq      = True
  TDot      == TDot     = True
  TPipe     == TPipe    = True
  TArity    == TArity   = True
  TComment  == TComment = True
  TIgnore   == TIgnore  = True
  _         == _        = False

TokenKind AATokenKind where
  TokType TName     = String
  TokType TEq       = ()
  TokType TDot      = ()
  TokType TPipe      = ()
  TokType TArity    = Nat
  TokType TComment  = ()
  TokType TIgnore   = ()

  tokValue TName    y = y
  tokValue TEq      y = ()
  tokValue TDot     y = ()
  tokValue TPipe    y = ()
  tokValue TArity   y = cast y
  tokValue TComment y = ()
  tokValue TIgnore  y = ()

AAToken : Type
AAToken = Token AATokenKind

Show AAToken where
  show (Tok kind text) = show kind ++ " " ++ show text

AAGrammar : Type -> Type
AAGrammar = Grammar () AAToken True

record ADTAlias where
  constructor MkADTAlias
  typeName : ExtName
  constructorNames : List (String, ExtName, Arity)

Show ADTAlias where
  show a = show (a.typeName, a.constructorNames)

||| Gramar for ADT definitions as in the description.
||| The format of the file:
|||
||| TypeName = package:Module.Path name
||| | ConstructorName = name arity
||| | ...
||| | ConstructorName = name arity
adtAlias : AAGrammar (String, ADTAlias)
adtAlias = do
  idrTypeName <- match TName
  match TEq

  pkg <- match TName
  mdl <- sepBy (match TDot) (match TName)

  hsTypeName <- match TName
  
  constNames <- map toList $ some $ do
    match TPipe
    idrCtrName <- match TName
    match TEq
    hsCtrName <- match TName
    arity <- match TArity
    pure (idrCtrName, MkExtName pkg mdl hsCtrName, arity)

  pure
    ( idrTypeName
    , MkADTAlias
      { typeName = MkExtName pkg mdl hsTypeName
      , constructorNames = constNames
      }
    )

||| The ADTAlias file can contain many ADT aliasing definitions
adtAliasFile : Grammar () AAToken False (List (String, ADTAlias))
adtAliasFile = many adtAlias

||| Parse as a content of an ADTAlias file, return Left on any errors.
|||
||| NOTE: The current implementation is still verbose on errors too.
parseAADefinitions : String -> Either String (List (String, ADTAlias))
parseAADefinitions str = do
  tks <- lexer str
  parser tks
  where
    lexer : String -> Either String (List (WithBounds AAToken))
    lexer str = case lex tokenMap str of
        (tokens, _, _, "") => Right tokens
        (tokens, _, _, rest) => Left "Non-lexed: \{rest}"
      where
        tokenMap : TokenMap AAToken
        tokenMap = toTokenMap
          [ (spaces, TIgnore)
          , ( alpha <+> (some (alphaNums <|> oneOf "-_'")), TName)
          , (exact "::", TName)
          , (exact ":", TName)
          , (exact "[]", TName)
          , (lineComment (exact "--"), TComment)
          , (exact "=", TEq)
          , (exact ".", TDot)
          , (exact "|", TPipe)
          , (digits, TArity)
          ]

    parser : List (WithBounds AAToken) -> Either String (List (String, ADTAlias))
    parser toks = case parse adtAliasFile $ filter (not . ignored) toks of
      Right (l, []) => Right l
      Right (e, ts) => Left "contains not consumed tokens: \{show ts}"
      Left e => Left (show e)
      where
        ignored : WithBounds AAToken -> Bool
        ignored (MkBounded (Tok TIgnore _) _ _) = True
        ignored (MkBounded (Tok TComment _) _ _) = True
        ignored _ = False

||| Check for duplicated Idris names.
checkAADefinitions : List (String, ADTAlias) -> Either String (List (String, ADTAlias))
checkAADefinitions as
  = checkDuplicated
  $ mergeMapsWith (+)
    [ SortedMap.singleton n (the Int 1)
    | (itn, alias) <- as
    , n <- itn :: (map (\(cn, _, _) => cn) alias.constructorNames)
    ]
  where
    -- A blunt merging
    mergeMapsWith : (Ord k) => (v -> v -> v) -> List (SortedMap k v) -> SortedMap k v
    mergeMapsWith f xs = foldl (mergeWith f) SortedMap.empty xs

    -- Duplicated means the occurence is greater than one, we return list because
    -- it is a nice monoid to aggregate values.
    keepDuplicated : String -> Int -> List String
    keepDuplicated n x = if x > 1 then [n] else []

    -- NOTE: Dependent foldMap access the keys too, casting is defined between non-dependent and
    -- dependent Maps
    checkDuplicated : SortedMap String Int -> Either String (List (String, ADTAlias))
    checkDuplicated ys = case the (List String) (Dependent.foldMap keepDuplicated (cast ys)) of
      [] => Right as
      ds => Left "Duplicated names: \{concat (intersperse ", " ds)}"

public export
NamespacePath : Type
NamespacePath = List String

public export
TypeName : Type
TypeName = String

public export
ConstructorName : Type
ConstructorName = String

export
ADTAliasFiles : Type
ADTAliasFiles = SortedMap NamespacePath (Maybe (SortedMap TypeName ExtName, SortedMap ConstructorName (ExtName, Arity)))

export
empty : ADTAliasFiles
empty = SortedMap.empty

renderQualified : List String -> String -> String
renderQualified m f = concat $ intersperse "." $ m ++ [f]

renderPath : List String -> String
renderPath m = concat (intersperse "/" m) ++ ".adt"

||| Tries to lookup the adt alias file only once from the given directory in its module path.
||| If the file it not found, it is recorded as missing and never looked up again.
||| If the file is found its content is parsed and mappings are stored in SortedMaps,
||| separated for TypeName and ConstructorNames.
readADTAliasFile
  :  ADTAliasFiles
  -> String
  -> NamespacePath
  -> Core (Maybe (SortedMap TypeName ExtName, SortedMap ConstructorName (ExtName, Arity)), ADTAliasFiles)
readADTAliasFile adtAliasFiles dir mdl = case lookup mdl adtAliasFiles of
  Just x => pure (x, adtAliasFiles)
  Nothing => do
    let path = renderPath (dir :: mdl)
    Right content <- coreLift $ readFile $ renderPath (dir :: mdl)
      | Left err => pure (Nothing, insert mdl Nothing adtAliasFiles)
    let Right adtAliasList
          = do { x <- parseAADefinitions content
               ; checkAADefinitions x
               }
        | Left err => coreFail $ InternalError "Resolving ADT Alias \{path} had a file error: \{show err}"   
    let typeNamesMap
          : SortedMap TypeName ExtName
          := fromList (map (\(idrTy, adtAlias) => (idrTy, adtAlias.typeName)) adtAliasList)
    let constNamesMap
          : SortedMap ConstructorName (ExtName, Arity)
          := fromList (concatMap (\(_, adtAlias) => (map (\(idrCtr, e, a) => (idrCtr, (e,a))) adtAlias.constructorNames)) adtAliasList)
    pure (Just (typeNamesMap, constNamesMap), insert mdl (Just (typeNamesMap, constNamesMap)) adtAliasFiles)

||| Tries to find an ADT alias for the given Idris type name.
export
typeName : ADTAliasFiles -> String -> NamespacePath -> TypeName -> Core (Maybe ExtName, ADTAliasFiles)
typeName adtAliasFiles dir mdl tn = do
  (Just (typesMap, _), adtAliasFiles') <- readADTAliasFile adtAliasFiles dir mdl
    | (Nothing, adtAliasFiles') => pure (Nothing, adtAliasFiles')
  pure (lookup tn typesMap, adtAliasFiles') -- TODO: Fix arity

||| Tries to find an ADT alias for the given Idris constructor name.
export
constructorName : ADTAliasFiles -> String -> NamespacePath -> ConstructorName -> Core (Maybe (ExtName, Arity), ADTAliasFiles)
constructorName adtAliasFiles dir mdl cn = do
  (Just (_, constructorMap), adtAliasFiles') <- readADTAliasFile adtAliasFiles dir mdl
    | (Nothing, adtAliasFiles') => pure (Nothing, adtAliasFiles')
  pure (lookup cn constructorMap,adtAliasFiles')
