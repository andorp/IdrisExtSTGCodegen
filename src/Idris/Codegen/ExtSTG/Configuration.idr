module Idris.Codegen.ExtSTG.Configuration

%default total

public export
data LogLevel
  = None
  | Debug
  | Message
  | Warning
  | Error

logLevelCode : LogLevel -> Nat
logLevelCode None    = 0
logLevelCode Debug   = 1
logLevelCode Message = 2
logLevelCode Warning = 3
logLevelCode Error   = 4

export
Eq LogLevel where
  a == b = logLevelCode a == logLevelCode b

export
Ord LogLevel where
  compare a b = compare (logLevelCode a) (logLevelCode b)

public export
record Configuration where
  constructor MkConfiguration
  foreignDirectory  : String
  logLevel          : LogLevel
