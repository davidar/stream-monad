-- https://gist.github.com/msullivan/4223fd47991acbe045ec
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Stream as Stream
import Control.Monad.Stream (Stream)
import Control.Monad.State (MonadState(..), StateT(..), execStateT, mapStateT)
import Test.Hspec (hspec, it, shouldBe)

type Var = Integer
type Subst = [(Var, Term)]
type State = (Subst, Integer)
type Program a = StateT State Stream a

data Term = Atom String | Pair Term Term | Var Var deriving (Eq, Show)

-- Apply a substitution to the top level of a term
walk :: Term -> Subst -> Term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just us -> walk us s
walk u s = u

extS :: Var -> Term -> Subst -> Subst
extS x v s = (x, v) : s

-- Try to unify two terms under a substitution;
-- return an extended subst if it succeeds
unify :: Term -> Term -> Subst -> Maybe Subst
unify u v s = un (walk u s) (walk v s)
  where un (Var x1) (Var x2) | x1 == x2 = return s
        un (Var x1) v = return $ extS x1 v s
        un u (Var x2) = return $ extS x2 u s
        un (Pair u1 u2) (Pair v1 v2) =
          do s' <- unify u1 v1 s
             unify u2 v2 s'
        un (Atom a1) (Atom a2) | a1 == a2 = return s
        un _ _  = mzero

fresh :: Program Term
fresh = do
  (s, c) <- get
  put (s, c+1)
  return (Var c)

-- microKanren program formers
zzz :: Program a -> Program a
zzz = mapStateT Stream.suspended

equiv :: Term -> Term -> Program ()
equiv u v = do
  (s, c) <- get
  case unify u v s of
    Nothing -> mzero
    Just s' -> put (s', c)

callFresh :: (Term -> Program a) -> Program a
callFresh = (fresh >>=)

disj :: Program a -> Program a -> Program a
disj = (<|>)

conj :: Program a -> Program b -> Program b
conj = (>>)

-- Recovering miniKanren interface
reify :: [State] -> [Term]
reify = map reifyState
  where
    reifyState :: State -> Term
    reifyState (s, _) = let v = walk' (Var 0) s in walk' v (reifyS v [])

    reifyS :: Term -> Subst -> Subst
    reifyS v s = case walk v s of
      Var v -> let n = reifyName (length s) in (v, n) : s
      Pair u v -> reifyS v $ reifyS u s
      _ -> s

    reifyName :: Int -> Term
    reifyName n = Atom $ "_." ++ show n

    walk' :: Term -> Subst -> Term
    walk' v s = case walk v s of
      Pair u v -> Pair (walk' u s) (walk' v s)
      v -> v

callEmptyState :: Program () -> Stream State
callEmptyState g = execStateT g ([], 0)

run :: Int -> (Term -> Program ()) -> [Term]
run n = reify . Stream.observeMany n . callEmptyState . callFresh

run' :: (Term -> Program ()) -> [Term]
run' = reify . Stream.observeAll . callEmptyState . callFresh

-- Tests
main :: IO ()
main = hspec $ do
  let ab = conj
        (callFresh (\a -> equiv a (Atom "7")))
        (callFresh (\b -> disj (equiv b (Atom "5")) (equiv b (Atom "6"))))
      five x = equiv x (Atom "5")
      fives x = disj (equiv x (Atom "5")) (zzz $ fives x)
      fivesRev x = disj (zzz $ fivesRev x) (equiv x (Atom "5"))
      sixes x = disj (equiv x (Atom "6")) (zzz $ sixes x)
      p56 x = disj (fives x) (sixes x)
      p010 q = do
        x <- fresh; y <- fresh
        equiv q (Pair x (Pair y x)) <|> equiv q (Pair y (Pair x y))
  it "ab" $ Stream.observeAll (callEmptyState ab) `shouldBe`
    [([(1,Atom "5"),(0,Atom "7")],2)
    ,([(1,Atom "6"),(0,Atom "7")],2)]
  it "five" $ run' five `shouldBe` [Atom "5"]
  it "fives" $ run 10 fives `shouldBe` replicate 10 (Atom "5")
  it "fivesRev" $ run 10 fivesRev `shouldBe` replicate 10 (Atom "5")
  it "p56" $ run 10 p56 `shouldBe` concat (replicate 5 [Atom "5",Atom "6"])
  it "null" $ run' (const $ pure ()) `shouldBe` [Atom "_.0"]
  it "p010" $ run 2 p010 `shouldBe`
    [Pair (Atom "_.0") (Pair (Atom "_.1") (Atom "_.0"))
    ,Pair (Atom "_.0") (Pair (Atom "_.1") (Atom "_.0"))]
