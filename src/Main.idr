module Main

import Compiler.ANF
import Compiler.Common
import Compiler.LambdaLift
import Compiler.VMCode
import Core.CompileExpr
import Core.Context
import Idris.Driver
import Idris.Syntax
import Language.JSON.Data
import System
import System.File
import System.Path
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal

import Idris.Codegen.ExtSTG.ANFToSTG
import Idris.Codegen.ExtSTG.Context
import Idris.Codegen.ExtSTG.JSON
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.StringTable

compile
  :  Ref Ctxt Defs
  -> Ref Syn SyntaxInfo
  -> (tmpDir : String)
  -> (outputDir : String)
  -> ClosedTerm
  -> (outfile : String)
  -> Core (Maybe String)
compile defs syn tmpDir outputDir term outfile = do
  coreLift $ putStrLn "Compile closed program term..."
  cdata <- getCompileData False ANF term
  stgCtxt <- mkSTGContext
  stgs  <- compileModule $ anf cdata
  let res = show $ toJSON stgs
  let out = outputDir </> outfile
  Right () <- coreLift $ writeFile out res
    | Left err => throw $ FileErr out err
  pure (Just out)

execute
  :  Ref Ctxt Defs
  -> Ref Syn SyntaxInfo
  -> (tmpDir : String)
  -> ClosedTerm
  -> Core ()
execute defs syn tmpDir term = do
  coreLift $ putStrLn "Maybe in an hour."

stgCodegen : Codegen
stgCodegen = MkCG compile execute Nothing Nothing

main : IO ()
main = mainWithCodegens [("stg", stgCodegen)]
