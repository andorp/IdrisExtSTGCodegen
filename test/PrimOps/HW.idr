module Main

import Data.Strings

data MyEither a b = MyLeft a | MyRight b

data MyDecEq : Type -> Type where
  MyYes : a           -> MyDecEq a
  MyNo  : (a -> Void) -> MyDecEq a

myDec1 : MyDecEq Char
myDec1 = MyYes 'a'

myDec2 : MyDecEq Char
myDec2 = MyNo ?absurd

test : Int
test = ?test1

test3 : Int -> Int -> Int
test3 x y = x

data MyStream : Type -> Type where
  MkMyStream : Lazy a -> Inf (MyStream a) -> MyStream a

mkStream : a -> MyStream a
mkStream x = MkMyStream x (mkStream x)

headS : MyStream a -> a
headS (MkMyStream x _) = x

tailS : MyStream a -> MyStream a
tailS (MkMyStream _ xs) = xs

data MyStream2 : Type -> Type where
  MkMyStream2 : a -> MyStream2 a -> MyStream2 a
  MkMyStream2N : MyStream2 a

myStream2 : a -> MyStream2 a
myStream2 x = MkMyStream2 x (myStream2 x)

heads2 : MyStream2 a -> Maybe a
heads2 MkMyStream2N = Nothing
heads2 (MkMyStream2 x _) = Just x

%foreign "stg:prim__consoleLog = base:Prelude.putStrLn"
prim__consoleLog : String -> PrimIO ()
export
consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x

%foreign "stg:prim__applyFnIO = base:Prelude.somewhat"
prim__applyFnIO : String -> Int -> (String -> Int -> PrimIO String) ->
                 PrimIO String
applyFnIO : HasIO io =>
            String -> Int -> (String -> Int -> IO String) -> io String
applyFnIO c i f = primIO $ prim__applyFnIO c i (\s, i => toPrim $ f s i)

data MyBuffer : Type -> Type where [external]

%foreign "stg:prim__myBuffer = base:Prelude.myBuffer"
prim__myBuffer : PrimIO (MyBuffer Int)
myBuffer : HasIO io => io (MyBuffer Int)
myBuffer = primIO prim__myBuffer

data ThreadID1 : Type where [external]

%foreign "stg:prim__fork = base:Something.something"
prim__fork : (1 prog : PrimIO ()) -> PrimIO ThreadID1

fork : (1 prog : IO ()) -> IO ThreadID1
fork act = fromPrim (prim__fork (toPrim act))

partial
main : IO ()
main = do
  name <- getLine
  let nm1 = the (MyEither String String) (MyLeft name)
  let md1 = the (MyDecEq Char) (MyYes 'a')
  putStrLn $ strTail $ "!Hello " ++ name
  case nm1 of
    MyLeft n => printLn $ fastUnpack n
    MyRight n => printLn $ fastUnpack n
  case md1 of
    MyYes a => printLn $ show a
    MyNo _ => printLn $ "Noooo!"
  printLn $ test3 1 test
  consoleLog "BLAH!"
  str <- applyFnIO "hello" 1 (\s, i => do { printLn (s,i); pure s})
  bfr <- myBuffer
  tid <- Main.fork (putStrLn "Hello Fork!")
  printLn $ headS $ tailS $ mkStream 4
  printLn $ heads2 $ myStream2 5
  pure ()



{-
Main.prim__applyFnIO = Foreign call ["stg:prim__applyFnIO = base:Prelude.somewhat"] [String, Int, String -> Int -> %World -> IORes String, %World] -> IORes
Main.{applyFnIO:1} = [0, 1, 2, 3, 4, 5, 6, 7]: %let v8 = (<Main.{applyFnIO:0} underapp 2>(v7, v0, v1, v2, v3, v4, v5, v6)) in (Main.prim__applyFnIO(v4, v5, v8, v7))
Main.applyFnIO = [0, 1, 2, 3, 4]: %case v1 of { %conalt Prelude.IO.HasIO at Prelude/IO.idr:33:1--35:32(v5, v6) => %let v7 = (v6 @ ([__])) in (%let v8 = (<Main.{applyFnIO:1} underapp 1>(v5, v6, v0, v1, v2, v3, v4)) in (v7 @ (v8))) Nothing }
-}
