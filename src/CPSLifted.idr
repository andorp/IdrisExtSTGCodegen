module CPSLifted

import Core.TT
import Core.FC
import Core.Name
import Core.Core
import Data.IORef
import Data.SortedMap
import Data.Vect
import Core.CompileExpr

-- Expriment based on jtobin.io/transforming-to-cps

-- Expr will be NamedCExp from Idris
-- AExpr and CExpr will ne namedCExp from Idris
-- Transformations must work on the NamedCExp as this is used by the JS backend first.
-- Manual example of tree algorithm and its CPS transform

data Expr : Type where
  Lam : (var : String) -> (body : Expr) -> Expr
  Var : (var : String) -> Expr
  App : (fun : Expr)   -> (arg : Expr) -> Expr

Show Expr where
  showPrec d (Lam var body) = showCon d "Lam" $ showArg var ++ showArg body
  showPrec d (Var var)      = showCon d "Var" $ showArg var
  showPrec d (App fun arg)  = showCon d "App" $ showArg fun ++ showArg arg

mutual

  -- Atomic expression
  -- Always produce a value and never cause side effects
  data AExpr
    = AVar String
    | ALam (List String) CExpr

  -- Complex expressions
  -- May not terminate, and they may produce side effects
  data CExpr
    = CApp AExpr (List AExpr)

mutual

  Show AExpr where
    showPrec d (AVar x) = showCon d "AVar" $ showArg x
    showPrec d (ALam xs x) = showCon d "ALam" $ showArg xs ++ showArg x

  Show CExpr where
    showPrec d (CApp x xs) = showCon d "CApp" $ showArg x ++ showArg xs

unique : IO (IO Int)
unique = do
  refCnt <- newIORef 0
  pure $ do
    x <- readIORef refCnt
    writeIORef refCnt (x + 1)
    pure x

mkGensym : IO (IO String)
mkGensym = map (map (\x => "v" ++ show x)) unique

mutual
  m : IO String -> Expr -> IO AExpr
  m gs (Lam var body) = do
    k       <- gs
    xformed <- tc gs body (AVar k)
    pure (ALam [var, k] xformed)
  m gs (Var var) = pure (AVar var)
  m gs (App fun arg) = pure $ AVar "..."

  tc : IO String -> Expr -> AExpr -> IO CExpr
  tc gs (Lam var body) c = do
    aexpr <- m gs (Lam var body)
    pure (CApp c [aexpr])
  tc gs (Var var) c = do
    aexpr <- m gs (Var var)
    pure (CApp c [aexpr])
  tc gs (App fun arg) c = do
    let cexpr = \fs => tk gs arg (\es => pure (CApp fs [es, c]))
    tk gs fun cexpr

  tk : IO String -> Expr -> (AExpr -> IO CExpr) -> IO CExpr
  tk gs (Lam var body) k = do
    aexpr <- m gs (Lam var body)
    k aexpr
  tk gs (Var var) k = do
    aexpr <- m gs (Var var)
    k aexpr
  tk gs (App fun arg) k = do
    rv <- gs
    xformed <- k (AVar rv)
    let cont = ALam [rv] xformed
        cexpr = \fs => tk gs arg (\es => pure (CApp fs [es, cont]))
    tk gs fun cexpr

test : Expr
test = App (Var "g") (Var "a")

test2 : Expr
test2 = Lam "x" (Var "x")

testIO : IO ()
testIO = do
  printLn test2
  gs <- mkGensym
  cexpr <- tc gs test2 (AVar "halt")
  printLn cexpr

||| This is some documention for the tree.
data Tree a = Leaf a | Branch a (Tree a) (Tree a)

depth : Tree a -> Nat
depth = loop where
  loop : Tree a -> Nat
  loop (Leaf a)       = 1
  loop (Branch a l r) =
    let dl = loop l
        dr = loop r
    in S (max dl dr)

{-
Just the body of the loop function:
NkNMFun "loop" ["t"]
  NmConCase
    (NmLocal "t")
    [ MkNConAlt "Leaf" ["a"]
      $ NmPrimVal 1
    , MkNConAlt "Branch" ["a", "l", "r"]
      $ NmLet "dl" (NmApp "loop" [NmLocal "l"])
      $ NmLet "dr" (NmApp "loop" [NmLocal "r"])
      $ NmApp "S"
        [ NmApp "max" [NmLocal "dl", NmLocal "dr"] ]
    ]
    Nothing

Main.2183:314:loop = [{arg:1}]:
  (%case !{arg:1}
    [ (%concase Main.Leaf Just 0 [{e:1}] (+Integer [1, 0]))
    , (%concase Main.Branch Just 1 [{e:3}, {e:4}, {e:5}]
        (%let dl (Main.2183:314:loop [!{e:4}])
        (%let dr (Main.2183:314:loop [!{e:5}])
        (+Integer [1, (Prelude.Types.max [!dl, !dr])]))))
    ]
    Nothing)
Main.depth = [{ext:0}]: (Main.2183:314:loop [!{ext:0}])
-}

depthC : Tree a -> Nat
depthC t = go t id where
  go : Tree a -> (Nat -> Nat) -> Nat
  go (Leaf a)       k = k 1
  go (Branch a l r) k = go l (\dl => go r (\dr => k (S (max dl dr))))

{-
MkNMFun "go" ["t", "k"]
  NmConCase
    (NmLocal "t")
    [ MkNConAlt "Leaf" ["a"]
      $ NmApp "k" [NmPrimVal "1"]
    , MkNConAlt "Branch" ["a", "l", "r"]
      $ NmApp "go"
        [ NmLocal "l"
        , NmLam "dl" $
          NmApp "go"
          [ NmLocal "r"
          , NmLam "dr" $
            NmApp "k" [ NmApp "S" [ NmApp "max" [ NmLocal "dl", NmLocal "dr" ] ] ]
          ]
        ]
    ]
    Nothing

Main.2183:314:go = [{arg:1}, {arg:2}, {arg:3}]:
  (%case !{arg:2}
    [ (%concase Main.Leaf   (Just 0) [{e:1}]
        (!{arg:3} [(+Integer [1, 0])]))
    , (%concase Main.Branch (Just 1) [{e:3}, {e:4}, {e:5}]
        (Main.2183:314:go
          [!{arg:1}, !{e:4},
            (%lam dl
              (Main.2183:314:go
                [!{arg:1}, !{e:5},
                  (%lam dr (!{arg:3}
                    [(+Integer [1, (Prelude.Types.max [!dl, !dr])])]))]))]))
    ]
    Nothing)
Main.depthC = [{arg:1}]: (Main.2183:314:go [!{arg:1}, !{arg:1}, (%lam x !x)])
-}

-- idFun : Core NamedCExp
-- idFun = do
--   n <- newUnique
--   pure
--     $ NmLam emptyFC n
--     $ NmLocal emptyFC n

{-
  -- Atomic expression
  -- Always produce a value and never cause side effects
  data AExpr
    = AVar String
    | ALam (List String) CExpr

  -- Complex expressions
  -- May not terminate, and they may produce side effects
  data CExpr
    = CApp AExpr (List AExpr)

-}

mutual

  data ACExp : Type where
    ANmLocal    : FC -> Name -> ACExp
    ANmRef      : FC -> Name -> ACExp
    ANmLam      : FC -> List Name -> CCExp -> ACExp
    ANmCon      : FC -> Name -> ConInfo -> Maybe Int -> List ACExp -> ACExp
    ANmPrimVal  : FC -> Constant -> ACExp
    ANmErased   : FC -> ACExp

  data CCExp : Type where
    CNmApp       : FC -> ACExp -> List ACExp -> CCExp
    CNmOp        : {ar : Nat} -> FC -> PrimFn ar -> Vect ar ACExp -> CCExp
    CNmExpPrim   : FC -> Name -> List ACExp -> CCExp
    CNmForce     : FC -> LazyReason -> CCExp -> CCExp
    CNmDelay     : FC -> LazyReason -> CCExp -> CCExp
    CNmCrash     : FC -> String -> CCExp
    CNmConCase   : FC -> ACExp -> List AConAlt -> Maybe ACExp -> CCExp
    CNmConstCase : FC -> ACExp -> List AConstAlt -> Maybe ACExp -> CCExp

  data AConAlt : Type where
    MkAConAlt : Name -> ConInfo -> (tag : Maybe Int) -> (args : List Name) -> CCExp -> AConAlt

  data AConstAlt : Type where
    MkAConstAlt : Constant -> CCExp -> AConstAlt

mutual

  fromAC : ACExp -> NamedCExp
  fromAC (ANmLocal fc x) = NmLocal fc x
  fromAC (ANmRef fc x) = NmRef fc x
  fromAC (ANmLam fc xs x) = foldl (\nc , name => NmLam fc name nc) (fromCC x) xs
  fromAC (ANmCon fc x y z xs) = NmCon fc x y z (map fromAC xs)
  fromAC (ANmPrimVal fc x) = NmPrimVal fc x
  fromAC (ANmErased fc) = NmErased fc

  fromCC : CCExp -> NamedCExp
  fromCC (CNmApp fc x xs) = NmApp fc (fromAC x) (map fromAC xs)
  fromCC (CNmOp fc x xs) = NmOp fc x (map fromAC xs)
  fromCC (CNmExpPrim fc x xs) = NmExtPrim fc x (map fromAC xs)
  fromCC (CNmForce fc x y) = NmForce fc x (fromCC y)
  fromCC (CNmDelay fc x y) = NmDelay fc x (fromCC y)
  fromCC (CNmCrash fc x) = NmCrash fc x
  fromCC (CNmConCase fc x xs y) = NmConCase fc (fromAC x) (map fromACA xs) (map fromAC y)
  fromCC (CNmConstCase fc x xs y) = NmConstCase fc (fromAC x) (map fromACnA xs) (map fromAC y)

  fromACA : AConAlt -> NamedConAlt
  fromACA (MkAConAlt x y tag args z) = MkNConAlt x y tag args (fromCC z)

  fromACnA : AConstAlt -> NamedConstAlt
  fromACnA (MkAConstAlt x y) = MkNConstAlt x (fromCC y)


mkUnique : IO (IO Name)
mkUnique = map (map (MN "v")) unique

mutual
  tcps : IO Name -> NamedCExp -> ACExp -> IO CCExp
  tcps gs v@(NmLocal fc x) k = pure (CNmApp fc k [!(tmi gs v)])
  tcps gs v@(NmRef fc x) k = pure (CNmApp fc k [!(tmi gs v)])
  tcps gs l@(NmLam fc x y) k = pure (CNmApp fc k [!(tmi gs l)])
  tcps gs (NmLet fc x y z) k = ?wat2_4 
  tcps gs (NmApp fc x xs) k = ?wat2_5 
  tcps gs (NmCon fc x y tag xs) k = ?wat2_6 
  tcps gs (NmOp fc x xs) k = ?wat2_7 
  tcps gs (NmExtPrim fc p xs) k = ?wat2_8 
  tcps gs (NmForce fc x y) k = ?wat2_9 
  tcps gs (NmDelay fc x y) k = ?wat2_10 
  tcps gs (NmConCase fc sc xs x) k = ?wat2_11 
  tcps gs (NmConstCase fc sc xs x) k = ?wat2_12 
  tcps gs (NmPrimVal fc x) k = ?wat2_13 
  tcps gs (NmErased fc) k = ?wat2_14 
  tcps gs (NmCrash fc x) k = ?wat2_15 

  tmi : IO Name -> NamedCExp -> IO ACExp
  tmi gs (NmLocal fc x) = pure (ANmLocal fc x)
  tmi gs (NmRef fc x) = pure (ANmRef fc x)
  tmi gs (NmLam fc var body) = do
    k <- gs
    xformed <- tcps gs body (ANmLocal fc var)
    pure (ANmLam fc [var,k] xformed)
  tmi gs (NmLet fc x y z) = ?wat3_4
  tmi gs (NmApp fc x xs) = ?wat3_5
  tmi gs (NmCon fc x y tag xs) = ?wat3_6
  tmi gs (NmOp fc x xs) = ?wat3_7
  tmi gs (NmExtPrim fc p xs) = ?wat3_8
  tmi gs (NmForce fc x y) = ?wat3_9
  tmi gs (NmDelay fc x y) = ?wat3_10
  tmi gs (NmConCase fc sc xs x) = ?wat3_11
  tmi gs (NmConstCase fc sc xs x) = ?wat3_12
  tmi gs (NmPrimVal fc x) = ?wat3_13
  tmi gs (NmErased fc) = ?wat3_14
  tmi gs (NmCrash fc x) = ?wat3_15

  tki : IO Name -> NamedCExp -> (ACExp -> IO CCExp) -> IO CCExp
  tki gs v@(NmLocal fc x) k = k !(tmi gs v)
  tki gs v@(NmRef fc x) k = k !(tmi gs v)
  tki gs v@(NmLam fc x y) k = k !(tmi gs v)
  tki gs (NmLet fc x y z) k = ?wat4_4
  tki gs (NmApp fc x xs) k = ?wat4_5
  tki gs (NmCon fc x y tag xs) k = ?wat4_6
  tki gs (NmOp fc x xs) k = ?wat4_7
  tki gs (NmExtPrim fc p xs) k = ?wat4_8
  tki gs (NmForce fc x y) k = ?wat4_9
  tki gs (NmDelay fc x y) k = ?wat4_10
  tki gs (NmConCase fc sc xs x) k = ?wat4_11
  tki gs (NmConstCase fc sc xs x) k = ?wat4_12
  tki gs (NmPrimVal fc x) k = ?wat4_13
  tki gs (NmErased fc) k = ?wat4_14
  tki gs (NmCrash fc x) k = ?wat4_15

testCExp : NamedCExp
testCExp
  = NmLam emptyFC (MN "x" 1)
  $ NmLocal emptyFC (MN "x" 1)

testNCPS : IO ()
testNCPS = do
  printLn testCExp
  gs <- mkUnique
  cexpr <- tcps gs testCExp (ANmLocal emptyFC (UN "halt"))
  printLn $ fromCC cexpr

{-
testLam : ACExp
testLam
  = ANmLam emptyFC [?someName1] ?wat5
--  $ ANmLocal emptyFC ?someName2

transformExpCPS : Name -> NamedCExp -> Core NamedCExp
transformExpCPS k t = ?wat1

transformNameConAltCPS : NamedConAlt -> Core NamedConAlt
transformNameConAltCPS (MkNConAlt x y tag args z) = do
  ?tnca

transformDefCPS : (Name, NamedDef) -> Core (List (Name, NamedDef))
transformDefCPS (originalFunction, MkNmFun args body) = do
  kontName <- newUnique
  goName <- newUnique
  x <- newUnique
  -- TODO: Rewrite arg names.
  pure
    [ ( goName
      , MkNmFun (args ++ [kontName]) !(transformExpCPS kontName body)
      )
    , ( originalFunction
      , MkNmFun args
        $ NmApp emptyFC (NmRef emptyFC goName)
          ((map (NmLocal emptyFC) args) ++ [NmLam emptyFC x (NmLocal emptyFC x)])
      )
    ]
transformDefCPS other = pure [other]
-}
