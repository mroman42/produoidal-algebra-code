module Distribution where

import Data.Bifunctor
import Text.XHtml (base, radio)
import Data.List ( groupBy , sortBy )
import Data.Maybe (listToMaybe)

-- Finitary (list-indexed) distribution monad
data Distribution a where
    Distribution :: { distribution :: [(a , Rational)] } -> Distribution a
  deriving (Show, Eq)


instance Functor Distribution where
  fmap :: (a -> b) -> (Distribution a -> Distribution b)
  fmap f = Distribution . fmap (first f) . distribution

weight :: Distribution a -> Rational
weight (Distribution xl) = sum [ p | (x , p) <- xl ]  

instance Applicative Distribution where
  pure :: a -> Distribution a
  pure x = Distribution [( x , 1 )]

  (<*>) :: Distribution (a -> b) -> Distribution a -> Distribution b
  (Distribution fl) <*> (Distribution xl) = Distribution
    [ (f x , p * q) | (f,p) <- fl , (x,q) <- xl ] 

instance Monad Distribution where
  (>>=) :: Distribution a -> (a -> Distribution b) -> Distribution b
  (Distribution xl) >>= f = Distribution
    [ (b , p * q)  | (x , p) <- xl , (b , q) <- distribution (f x) ]

aggregate :: (Eq a, Ord a) => Distribution a -> Distribution a
aggregate = 
  Distribution 
  . concatMap total 
  . groupBy (\(a,_) (b,_) -> a == b)
  . sortBy (\(a,_) (b,_) -> compare a b)
  . distribution

total :: (Num r) => [(a,r)] -> [(a,r)]
total l = maybe [] pure $ do
  let r = (sum . fmap snd) l
  a <- listToMaybe l
  return (fst a,r)
    
normalize :: Distribution a -> Distribution a
normalize l@(Distribution xl) = Distribution [ (x , r / weight l) | (x , r) <- xl ]

binomial :: Rational -> Distribution Bool
binomial r = Distribution [(True , r) , (False , 1-r)]