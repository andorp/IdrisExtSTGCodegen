module Main -- where

import Compiler.ANF
import Compiler.Common
import Compiler.LambdaLift
import Compiler.VMCode
import Core.CompileExpr
import Core.Context
import Idris.Driver
import Language.JSON.Data
import System
import System.File
import Text.PrettyPrint.Prettyprinter.Doc
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Utils.Path

import Idris.Codegen.ExtSTG.ANFToSTG
import Idris.Codegen.ExtSTG.ExternalTopIds
import Idris.Codegen.ExtSTG.JSON
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.StringTable

compile
  :  Ref Ctxt Defs
  -> (tmpDir : String)
  -> (outputDir : String)
  -> ClosedTerm
  -> (outfile : String)
  -> Core (Maybe String)
compile defs tmpDir outputDir term outfile = do
  coreLift $ putStrLn "Compile closed program term..."
  cdata <- getCompileData ANF term
  cntr  <- mkCounter
  unqs  <- mkUniques
  dts   <- mkDataTypes
  ebs   <- mkExternalBinders
  st    <- newStringTableRef
  stgs  <- compileModule $ anf cdata
  let res = show $ toJSON stgs
  let out = outputDir </> outfile
  Right () <- coreLift $ writeFile out res
    | Left err => throw $ FileErr out err
  pure (Just out)

execute
  :  Ref Ctxt Defs
  -> (tmpDir : String)
  -> ClosedTerm
  -> Core ()
execute defs tmpDir term = do
  coreLift $ putStrLn "Maybe in an hour."

stgCodegen : Codegen
stgCodegen = MkCG compile execute

main : IO ()
main = mainWithCodegens [("stg", stgCodegen)]
