module Idris.Codegen.ExtSTG.ForeignFile

import Core.Context
import Core.Name
import Data.SortedMap
import System.File
import Text.Lexer
import Text.Parser

import Idris.Codegen.ExtSTG.ExtName

%default total

-- It can not be assumed that the developer is able to add the necessary foreign
-- lines in all the libraries. Eg no access to the code repository or the
-- maintainers of the library don't want to take maintenance cost for the
-- Foreign definitions for the STG backend. To this end we can define the
-- foreign definitions locally to the project we compile and we can add the
-- FFI strings in corresponding files. Such as .foreign/Data/Strings.stgffi.

-- The compiler first tries to find the ffi definition in the Idris file, if
-- not found then the compiler looks the definition in the stgffi file.
-- Every references file is loaded only once.

public export
ModulePath : Type
ModulePath = List String

public export
FunName : Type
FunName = String

export
FFIFiles : Type
FFIFiles = SortedMap ModulePath (SortedMap FunName ExtName)

export
empty : FFIFiles
empty = SortedMap.empty

data FFITokenKind
  = TString
  | TEq
  | TColon
  | TDot
  | TComment
  | TIgnore

Eq FFITokenKind where
  TString   == TString  = True
  TEq       == TEq      = True
  TColon    == TColon   = True
  TDot      == TDot     = True
  TComment  == TComment = True
  TIgnore   == TIgnore  = True
  _         == _        = False

TokenKind FFITokenKind where
  TokType TString  = String
  TokType TEq      = ()
  TokType TColon   = ()
  TokType TDot     = ()
  TokType TComment = ()
  TokType TIgnore  = ()

  tokValue TString  x = x
  tokValue TEq      _ = ()
  tokValue TColon   _ = ()
  tokValue TDot     _ = ()
  tokValue TComment _ = ()
  tokValue TIgnore  _ = ()

FFIToken : Type
FFIToken = Token FFITokenKind

Show FFIToken where
    show (Tok kind text) = text

FFIGrammar : Type -> Type
FFIGrammar = Grammar () FFIToken True

extNameDef : FFIGrammar ExtName
extNameDef = do
  mdl  <- match TString
  match TColon
  path <- sepBy1 (match TDot) (match TString)
  pure (MkExtName mdl (init path) (last path))

ffiDefinition : FFIGrammar (String, ExtName)
ffiDefinition = do
  fn <- match TString
  match TEq
  en <- extNameDef
  pure (fn, en)

ffiFile : FFIGrammar (List (String, ExtName))
ffiFile = map (catMaybes . toList) $ some (map (const Nothing) (match TComment) <|> map Just ffiDefinition)

parseFFIDefinitions : String -> Either String (List (String, ExtName))
parseFFIDefinitions str = do
  tks <- lexer str
  parser tks
  where
    lexer : String -> Either String (List (WithBounds FFIToken))
    lexer str = case lex tokenMap str of
        (tokens, _, _, "") => Right tokens
        (tokens, _, _, rest) => Left "Non-lexed: \{rest}"
      where
        tokenMap : TokenMap FFIToken
        tokenMap = toTokenMap
          [ (spaces, TIgnore)
          , (some (alphaNums <|> oneOf "_'"), TString)
          , (lineComment (is '#'), TComment)
          , (exact "=", TEq)
          , (exact ":", TColon)
          , (exact ".", TDot)
          ]  

    parser : List (WithBounds FFIToken) -> Either String (List (String, ExtName))
    parser toks = case parse ffiFile $ filter (not . ignored) toks of
      Right (l, []) => Right l
      Right (e, ts) => Left "contains not consumed tokens: \{show ts}"
      Left e => Left (show e)
      where
        ignored : WithBounds FFIToken -> Bool
        ignored (MkBounded (Tok TIgnore _) _ _) = True
        ignored _ = False

renderQualified : List String -> String -> String
renderQualified m f = concat $ intersperse "." $ m ++ [f]

renderPath : List String -> String
renderPath m = concat (intersperse "/" m) ++ ".stgffi"

export
covering
ffiExtName : FFIFiles -> String -> ModulePath -> FunName -> Core (ExtName, FFIFiles)
ffiExtName ffiFiles dir mdl fun =
  case lookup mdl ffiFiles of
    Just funs => case lookup fun funs of
      Nothing => coreFail $ InternalError "FFI definition is not found for \{renderQualified mdl fun}"
      Just e => pure (e, ffiFiles)
    Nothing => do
      Right content <- coreLift $ readFile $ renderPath (dir :: mdl)
        | Left err => coreFail $ InternalError "Resolving FFI function \{renderPath (dir :: mdl)} had a file error: \{show err}"
      let Right extNamesList = parseFFIDefinitions content
          | Left err => coreFail $ InternalError "Resolving FFI function \{renderPath (dir :: mdl)} had a file error: \{show err}"
      let extNamesMap : SortedMap String ExtName := fromList extNamesList
      let Just extName = lookup fun extNamesMap
          | Nothing => coreFail $ InternalError "FFI definition in file is not found for \{renderQualified mdl fun}"
      pure (extName, insert mdl extNamesMap ffiFiles)
