-- https://gist.github.com/msullivan/4223fd47991acbe045ec
import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Stream as Stream
import Control.Monad.Stream (Stream)
import Control.Monad.State (MonadState(..), StateT(..), execStateT, mapStateT)
import Test.Hspec

type Var = Integer
type Subst = [(Var, Term)]
type State = (Subst, Integer)
type Program a = StateT State Stream a

data Term = Atom String | Pair Term Term | Var Var deriving (Eq, Show)

-- Apply a substitution to the top level of a term
walk (Var v) s = case lookup v s of Nothing -> Var v
                                    Just us -> walk us s
walk u s = u

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

-- MicroKanren program formers
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


five = callFresh (\x -> equiv x (Atom "5"))

fives_ x = disj (equiv x (Atom "5")) (zzz $ fives_ x)
fives = callFresh fives_

fivesRev_ x = disj (zzz $ fivesRev_ x) (equiv x (Atom "5"))
fivesRev = callFresh fivesRev_

ab = conj
  (callFresh (\a -> equiv a (Atom "7")))
  (callFresh (\b -> disj (equiv b (Atom "5")) (equiv b (Atom "6"))))

runTest p = Stream.observeAll (execStateT p ([], 0))

main = hspec $ do
  it "five" $ runTest five `shouldBe` [([(0,Atom "5")],1)]
  it "fives" $ take 10 (runTest fives) `shouldBe` replicate 10 ([(0,Atom "5")],1)
  it "fivesRev" $ take 10 (runTest fivesRev) `shouldBe` replicate 10 ([(0,Atom "5")],1)
  it "ab" $ runTest ab `shouldBe`
    [([(1,Atom "5"),(0,Atom "7")],2),([(1,Atom "6"),(0,Atom "7")],2)]
