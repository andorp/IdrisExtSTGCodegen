module Idris.Codegen.ExtSTG.Configuration

public export
data LogLevel
  = None
  | Debug
  | Message
  | Warning
  | Error

export
Eq LogLevel where
  None    == None     = True
  Debug   == Debug    = True
  Message == Message  = True
  Warning == Warning  = True
  Error   == Error    = True
  _       == _        = False

export
Ord LogLevel where
  compare None    None    = EQ
  compare None    _       = LT
  compare Debug   None    = GT
  compare Debug   Debug   = EQ
  compare Debug   _       = LT
  compare Message None    = GT
  compare Message Debug   = GT
  compare Message Message = EQ
  compare Message _       = LT 
  compare Warning None    = GT
  compare Warning Debug   = GT
  compare Warning Message = GT
  compare Warning Warning = EQ
  compare Warning Error   = LT
  compare _       _       = EQ

public export
record Configuration where
  constructor MkConfiguration
  foreignDirectory  : String
  logLevel          : LogLevel
