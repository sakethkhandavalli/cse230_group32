{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Bomberman
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead, food, score, snake
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _bomberman  :: Coord        -- ^ bomberman location
  , _dir        :: Direction    -- ^ direction
  , _bomb       :: Coord        -- ^ bomb location
  , _explosion  :: Stream Coord -- ^ explosion locations
  , _enemies    :: Stream Coord -- ^ enemies locations
  , _bricks     :: Stream Coord -- ^ bricks locations
  , _walls      :: Stream Coord -- ^ walls locations
  , _target     :: Coord        -- ^ Target location
  , _dead       :: Bool         -- ^ game over flag
  , _lives      :: Int          -- ^ remaining lives
  , _score      :: Int          -- ^ score
  , _locked     :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 20

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do

  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False

  -- die (moved into boundary), eat (moved into food), or move (move into space)
  MaybeT (Just <$> modify move)

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g@Game { _bomberman = b }     = g & (b & _y %~ (\y -> (y + 1))) 
move _                             = error "Snakes can't be empty!"

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  walls <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _bomberman  = (V2 0 0)
        , _walls  = walls
        , _score  = 0
        , _dir    = North
        , _dead   = False
        , _locked = False
        }
  return $ execState . g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
