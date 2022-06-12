module Main

data TVar : (0 _ : Type) -> Type where [external]

data STMWorld : Type where [external] -- VoidRep on Haskell side

data STMRes : Type -> Type where
  MKSTMRes : (result : a) -> (1 x : STMWorld) -> STMRes a

STM : Type -> Type
STM a = (1 x : STMWorld) -> STMRes a

%foreign "stg:stm:atomically"
atomically : STM a -> PrimIO a

%foreign "stg:stm:newTVar"
newTVar : a -> STM (TVar a)

%foreign "stg:stm:readTVar"
readTVar : TVar a -> STM a

-- %foreign "stg:stm:bind"
-- bind : STM a -> (a -> STM b) -> STM b

-- (>>=) : STM a -> (a -> STM b) -> STM b
-- (>>=) = bind

-- PrimIO

-- public export
-- data IORes : Type -> Type where
--      MkIORes : (result : a) -> (1 x : %World) -> IORes a

--   public export
--    13  PrimIO : Type -> Type
--    14: PrimIO a = (1 x : %World) -> IORes a

-- %foreign "stg:stm:pure"
-- pure : a -> STM a

-- main : IO ()
-- main = do
--   x <- primIO $ atomically $ do
--           v <- newTVar (the Int 1)
--           readTVar v
--   printLn x
