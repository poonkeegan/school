module A3DI where

import Control.Monad (liftM, ap)

-- You can import more modules/functions here.

-- The API for reading a character with possible failure.
class Monad f => MonadGetChar f where
    -- Read and return a character. Throw EOF if no more to read.
    get :: f Char
    -- User can throw an exception manually.
    throw :: Exception -> f a
    -- Catch and handle an exception.
    catch :: f a -> (Exception -> f a) -> f a

-- Possible exceptions.
data Exception = EOF | Other String deriving (Eq, Show)

-- Example usage of the API.
albertProg :: MonadGetChar f => f String
albertProg = do
    c1 <- get
    c2 <- get
    if [c1, c2] == "AL"
        then return "good header"
        else return "wrong header"
  `catch`
      \e -> throw (Other "input too short")

-- Pick one to uncomment.
-- newtype Feeder a = FeederOf ([Char] -> Either ([Char], Exception a))
newtype Feeder a = FeederOf ([Char] -> ([Char], Either Exception a))
-- newtype Feeder a = FeederOf ([Char] -> Either Exception a)

-- Run the Feeder program with the string as input source.
-- Return a Left for uncaught exception, a Right for normal return.
runFeeder :: Feeder a -> String -> Either Exception a
runFeeder = (\feeder -> \initialState -> case feeder of 
                                           FeederOf feedtype -> snd(feedtype initialState))
testFeeder = (\feeder -> \initialState -> case feeder of 
                                           FeederOf feedtype -> feedtype initialState)

keeganProg = do
    throw (Other "test")

testAlbertProg :: String -> Either Exception String
testAlbertProg = runFeeder albertProg

testKeeganProg = testFeeder keeganProg

test1 = testAlbertProg "ALBERT"
test2 = testAlbertProg "LABERT"
test3 = testAlbertProg "A"

instance Functor Feeder where
    fmap = liftM
instance Applicative Feeder where
    pure a = return a
    (<*>) = ap

instance Monad Feeder where
  return a = FeederOf (\state -> (state, Right a))
  FeederOf prog >>= nextProg = FeederOf (\initialState -> case prog initialState of
                                            (nextState, Right value) -> case nextProg value of
                                                FeederOf newProg -> newProg nextState
                                            (nextState, Left except) -> (nextState, Left except))

instance MonadGetChar Feeder where
    get = FeederOf (\unconsumedInput -> case unconsumedInput of
                                          [] -> ([], Left EOF)
                                          (x:xs) -> (xs, Right x))
    throw except = FeederOf (\input -> (input, Left except))
    catch (FeederOf program) error = FeederOf (\initialState -> case program initialState of
                                                      (newState, Left excpt) -> case error excpt of
                                                          FeederOf newProg -> newProg newState
                                                      (newState, Right value) -> (newState, Right value))
