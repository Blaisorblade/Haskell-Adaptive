-- -*- haskell-hugs-program-args: ("+." "-98") -*-
{-# LANGUAGE FlexibleContexts #-}

-- A demo program of the Adaptive library, implementing a simple
-- spreadsheet.  Requires a VT100-like terminal to work.  Expressions
-- have to be entered according to the Expr datatype.

-- Magnus Carlsson, magnus@cse.ogi.edu

import Control.Monad.Adaptive
import Data.Char
import Control.Monad.Adaptive.Ref
import Control.Monad(ap,when)
import Data.IORef(IORef)
import System.Exit

type InIO m a = m IO IORef a
type IOMod a = InIO Modifiable a

data CellRef = CR String (IOMod Integer) deriving Eq
instance Show CellRef where show (CR s _) = s

data Expr c = Const Integer | Add (Expr c) (Expr c) | Cell c
  deriving (Eq,Read,Show)

eval :: Expr CellRef -> InIO Changeable Integer
eval (Const i)       = return i
eval (Add e1 e2)     = return (+) `ap` eval e1 `ap` eval e2
eval (Cell (CR _ n)) = readMod n

memo ma = readMod =<< newMod ma

instance Eq (a -> b) where a == b = False

ap' mf ma = do
  m <- newMod mf
  a <- memo ma
  f <- readMod m
  return (f a)

newCell :: NewMod m IO IORef => 
           String -> InIO m (IOMod (Expr CellRef), CellRef)
newCell s = do
     c <- newMod (return (Const 0))
     v <- newMod $ readMod c >>= eval
     return (c,CR s v)

newCell' n = do
    let s = "c" ++ show n
    inM $ prAt (n+2) 0 3 (s++": ")
    a@(c,CR s v) <- newCell s
    newMod $ readMod v >>= inM . prAt (n+2) 5 10 . show
    newMod $ readMod c >>= inM . prAt (n+2) 15 40 . show
    return (s,a)

prAt l c w s = putStr (pos l c ++ replicate w ' ' ++ pos l c++s)
esc = ("\ESC["++)
pos l c = esc (show l++";"++show c++"H")
clear = pos 0 0 ++ esc "J"
cleareol = esc "K"

readPrompt c s = do prAt 20 c 0 (s++"> "++ cleareol)
                    s <- getLine
                    when (s == "quit") $ exitWith ExitSuccess
                    return s

msg s = prAt 19 0 0 (s ++ cleareol)

prompt env = inM p where
   p = do s <- readPrompt 0 "Cell"
          case lookup s env of
            Nothing -> do msg ("Cell " ++ show s ++ " not found")
                          p
            Just (c,v) -> do let r = do s <- readPrompt 10 "Expr"
                                        case reads s of 
                                          [(e,"")] -> msg "" >> return (c,e)
                                          _ -> do msg "Syntax error"
                                                  r
                             r

data CellName = CN String
instance Read CellName where readsPrec _ s = [(CN $ takeWhile isAlphaNum s',
                                               dropWhile isAlphaNum s')]
                                  where s' = dropWhile isSpace s

instance Show CellName where show (CN s) = s

subst m env (Const i)     = Const i
subst m env (Add e1 e2)   = Add (subst m env e1) (subst m env e2)
subst m env (Cell (CN s)) = Cell $ case lookup s env of
                                            Nothing    -> m
                                            Just (c,v) -> v

main :: IO ()
main = run $ do
     inM $ putStr clear
     env <- mapM newCell' [0..9]
     m0 <- CR "?" `fmap` newMod (return 0)
     let loop = do (c,e) <- prompt env
                   let e' = subst m0 env e
                   change c e'
                   propagate
                   loop
     loop

-- small non-interactive example

newCellPr s = do
     a@(c,CR s v) <- newCell s
     newMod $ do e <- readMod c
                 x <- readMod v
                 inM $ putStrLn (s++" = "++show e ++ " = " ++ show x)
     return a

test = run $ do
     [(c1,v1),(c2,v2)] <- mapM newCellPr ["c1","c2"]
     change c1 (Const 10)
     change c2 (Add (Cell v1) (Const 5))
     inM (putStrLn "Propagate") >> propagate
     change c1 (Add (Cell v2) (Const 4))
     change c2 (Const 1)
     inM (putStrLn "Propagate") >> propagate
     change c2 (Const 2)
     inM (putStrLn "Propagate") >> propagate
