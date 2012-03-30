
This is a reproduction of [Wolf-pack (*Canis lupus*) hunting strategies emerge
from simple rules in computational simulations][wolfpack].

\begin{code}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import           Control.Monad
\end{code}

First, we need some data structures for the various things in the application.

`Pos` is an x-y coordinate in the world, which is an infinite two-dimensional
plane. There are also some accessors for this.

\begin{code}
type Pos = (Double, Double)

x :: Pos -> Double
x = fst

y :: Pos -> Double
y = snd
\end{code}

`Wolf` tracks where the wolf is, its direction and speed (tracked by
delta-axis). The wolf also has an individual safe distance from its target.

\begin{code}
data Wolf = Wolf
    { wolfPos      :: Pos
    , wolfDX       :: Double
    , wolfDY       :: Double
    , wolfSafeDist :: Double
    } deriving (Show)
\end{code}

`Prey` tracks the generic prey. It has all the same information that the `Wolf`
does, but they're treated so differently, it made sense having separate data
types. This way, the compiler can help us more.

\begin{code}
data Prey = Prey
    { preyPos      :: Pos
    , preyDX       :: Double
    , preyDY       :: Double
    , preySafeDist :: Double
    } deriving (Show)
\end{code}

`Env` wraps all of these up into a state of the environment. It also includes a
distance matrix.

\begin{code}
type PosPair = (Pos, Pos)
type DistMatrix = M.HashMap PosPair Double

data Env = Env
    { envGeneration :: Int
    , envWolves     :: [Wolf]
    , envPrey       :: [Prey]
    , envDistances  :: DistMatrix
    } deriving (Show)
\end{code}

## Distance Matrix

We need a lookup function for the distance matrix, since distances can be
reciprocal, and we only want to calculate or fill in the matrix with one item.
(I may need to revisit this. It may be faster to populate the matrix with
things going each direction.)

\begin{code}
lookupDist :: DistMatrix -> Pos -> Pos -> Maybe Double
lookupDist matrix x y = msum [ M.lookup (x, y) matrix
                             , M.lookup (y, x) matrix
                             ]
\end{code}

This takes a list of wolves and prey and returns a new distance matrix for
them.

\begin{code}
makeDistanceMatrix :: [Wolf] -> [Prey] -> DistMatrix
makeDistanceMatrix wolves prey =
    M.fromList . concatMap (uncurry pairDists) . listPairs $ map dist poss
    where
        poss = S.fromList $ (map wolfPos wolves) ++ (map preyPos prey)

        pairDists :: Pos -> [Pos] -> [Double]
        pairDists p = map (dist p)

        dist :: Pos -> Pos -> Double
        dist (x1, y1) (x2, y2) = sqrt $ dx^2 + dy^2
            where dx = x2 - x1
                  dy = y2 - y1
\end{code}

This is a utility function that takes a list of items and returns a stream of
pairs of the head and the tails over the list, while there are any of those.

\begin{code}
listPairs :: [a] -> [(a, [a])]
listPairs []    = []
listPairs (a:b) = (a, b) : distPairs b
\end{code}

The processing steps will go like this:

1. init

\begin{code}
main :: IO ()
main = do
    putStrLn "Wolf Hunt!"
\end{code}

[wolfpack]: http://www.sciencedirect.com/science/article/pii/S0376635711001884 "Wolf-pack hunting strategies..."

