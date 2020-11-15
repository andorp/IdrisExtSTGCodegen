module Main -- where

import Core.Context
import Compiler.Common
import Idris.Driver
import Core.CompileExpr
import Compiler.LambdaLift
import Compiler.ANF
import Compiler.VMCode
import Utils.Path
import System
import System.File
import Idris.Codegen.ExtSTG.TTtoSTG
import Idris.Codegen.ExtSTG.STG
import Idris.Codegen.ExtSTG.JSON
import Idris.Codegen.ExtSTG.Pretty
import Text.PrettyPrint.Prettyprinter.Render.Terminal
import Text.PrettyPrint.Prettyprinter.Doc
import Language.JSON.Data

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
