{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Bomberman
  ( initGame
  , Game(..)
  , Direction(..)
  , step
  , moveBomberman, getBombLocs, plantBomb
  , walls, bomberman, brickwalls, bombs, explosions, score, dead, enemies, target
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
import System.Random (getStdRandom, randomR)

-- Types

data Game = Game
  { _bomberman  :: Coord        -- ^ bomberman location
  , _bombs      :: [Bomb]       -- ^ bombs location
  , _explosions :: [Coord]      -- ^ explosion locations
  , _enemies    :: [Coord]      -- ^ enemies locations
  , _brickwalls :: [Coord]      -- ^ bricks locations
  , _walls      :: Seq Coord    -- ^ walls locations
  , _target     :: Coord        -- ^ Target location
  , _dead       :: Bool         -- ^ game over flag
  , _lives      :: Int          -- ^ remaining lives
  , _score      :: Int          -- ^ score
  , _locked     :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Bomb = (Coord, Int)

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
height = 21
width = 21

-- Varies between 0 and 1, increasing the value leads to creation of more num. of bricks
brickDensity :: Double
brickDensity = 0.1

enemyDensity :: Double
enemyDensity = 0.03

-- Bomb timer: no. of ticks before explosion
bombTimer = 3

-- scores when an enemy/brick is killed
scorePerBrick = 2
scorePerEnemy = 10

-- Functions

-- | Step forward in time
-- |  - update score
step :: Game -> IO Game
step g = if (g ^. dead) then (return g)
        else do
              let updatedGame = (updateBricks g (g ^. brickwalls))
                                & explosions .~ []
                                & (updateBombs (g ^. bombs))
              updatedGame <- (updateEnemies (updatedGame ^. enemies) updatedGame)
              return ((killEnemies updatedGame (updatedGame ^. enemies)) & checkDeath)

updateBricks :: Game -> [Coord] -> Game
updateBricks g [] = g & brickwalls .~ []
updateBricks g (b: rest) = if (checkExplosion g b)
                           then restGame & score %~ (\x -> (x+scorePerBrick))
                           else restGame & brickwalls %~ (++ [b])
  where
    restGame = updateBricks g rest

updateBombs :: [Bomb] -> Game -> Game
updateBombs [] g = g & bombs .~ []
updateBombs (b@(loc, timer): rest) g = if (timer == 0)
                                      then restGame & explosions %~ (++ (getExplosionLocs loc))
                                      else restGame & bombs %~ (++ [(loc, timer-1)])
  where
    restGame = updateBombs rest g

getExplosionLocs :: Coord -> [Coord]
getExplosionLocs (V2 x y) = [(V2 x y), (V2 (x-1) y), (V2 x (y-1)), (V2 (x+1) y), (V2 x (y+1))]

-- | Move bomberman
-- | TODO: add locked true
moveBomberman :: Direction -> Game -> Game
moveBomberman d g@Game { _bomberman = b } = if (g ^. dead)
                                            then g
                                            else g & bomberman %~ (moveDir g d)

moveDir :: Game -> Direction -> Coord -> Coord
moveDir g d prev
  | d == North = prev & _y %~ (\y -> if (checkObstacle g prev 0 1) then y else (y + 1))
  | d == South = prev & _y %~ (\y -> if (checkObstacle g prev 0 (-1)) then y else (y - 1))
  | d == East  = prev & _x %~ (\x -> if (checkObstacle g prev 1 0) then x else (x + 1))
  | d == West  = prev & _x %~ (\x -> if (checkObstacle g prev (-1) 0) then x else (x - 1))

-- Takes current coord, change in x, change in y and checks if obstacle is present at new coords
checkObstacle :: Game -> Coord -> Int -> Int -> Bool
checkObstacle g prev@(V2 x y) dx dy = if ((checkWall newC) ||
                                          (checkBrick g newC) ||
                                          (checkBomb g newC) ||
                                          (checkEnemy g newC))
                                      then True
                                      else False
  where
    newC = (V2 (x+dx) (y+dy))

-- | Set a valid next food coordinate
nextFood :: State Game ()
nextFood = do
  dead .= False

getBricks :: IO [Coord]
getBricks = do
              list <- genBricks allCoords
              return list
  where
    allCoords = [(V2 x y) | x <- [0..width-1], y <- [0..height-1]]

genBricks :: [Coord] -> IO [Coord]
genBricks []        = (return [])
genBricks (a: rest) = do
                        isBrick <- (genBrick a)
                        restBricks <- (genBricks rest)
                        if isBrick
                        then (return (a: restBricks))
                        else (return restBricks)

genBrick :: Coord -> IO Bool
genBrick c@(V2 x y) = do
                        randomNum <- (drawDouble 0 1)
                        if (checkWall c) then (return False)
                        else if ((x == 1) && (y==1)) then (return False)
                        else if (randomNum < brickDensity) then (return True)
                        else (return False)
                          
drawDouble :: Double -> Double  -> IO Double
drawDouble x y = getStdRandom (randomR (x,y))

getWalls :: Seq Coord
getWalls = S.fromList $ checkWalls allCoords
  where
    allCoords = [(V2 x y) | x <- [0..width-1], y <- [0..height-1]]

checkWalls :: [Coord] -> [Coord]
checkWalls []        = []
checkWalls (a: rest) = if (checkWall a)
                        then (a: checkWalls rest)
                        else (checkWalls rest)

checkBrick :: Game -> Coord -> Bool
checkBrick g c = c `elem` g ^. brickwalls

checkWall :: Coord -> Bool
checkWall (V2 x y) = if ((x == 0) || (x == width-1)) then True
                      else if ((y == 0) || (y == height-1)) then True
                      else if ((x `mod` 2 == 0) && (y `mod` 2 == 0)) then True
                      else False

checkBomb :: Game -> Coord -> Bool
checkBomb g c = c `elem` bombLocs
  where
    bombLocs = getBombLocs g

checkExplosion :: Game -> Coord -> Bool
checkExplosion g c = c `elem` g ^. explosions

getBombLocs :: Game -> [Coord]
getBombLocs g = getLocs (g ^. bombs)

getLocs :: [Bomb] -> [Coord]
getLocs [] = []
getLocs ((coord, _): rest) = (coord : getLocs rest)

plantBomb :: Game -> Game
plantBomb g@Game { _bomberman = b } = if (checkBomb g b) then g
                                      else g & bombs %~ (++ [(b, bombTimer)])

checkDeath :: Game -> Game
checkDeath g = if ((g ^. bomberman) `elem` (g ^. enemies)) || ((g ^. bomberman) `elem` (g ^. explosions))
               then g & dead .~ True
               else g

--ENEMIES

getEnemy :: [Coord] -> IO [Coord]
getEnemy brickwalls = do
              list <- genEnemies allCoords brickwalls
              return list
  where
    allCoords = [(V2 x y) | x <- [0..width-1], y <- [0..height-1]]

genEnemies :: [Coord] -> [Coord] -> IO [Coord]
genEnemies [] _       = (return [])
genEnemies (a: rest) brickwalls = do
                        isEnemy <- (genEnemy a brickwalls)
                        restEnemies <- (genEnemies rest brickwalls)
                        if isEnemy
                        then (return (a: restEnemies))
                        else (return restEnemies)

genEnemy :: Coord -> [Coord] -> IO Bool
genEnemy c@(V2 x y) brickwalls = do
                        randomNum <- (drawDouble 0 1)
                        if (checkWall c) then (return False)
                        else if (c `elem` brickwalls) then (return False)
                        else if ((x == 1) && (y==1)) then (return False)
                        else if (randomNum < enemyDensity) then (return True)
                        else (return False)

checkEnemy :: Game -> Coord -> Bool
checkEnemy g c = c `elem` g ^. enemies

updateEnemies :: [Coord] -> Game -> IO Game
updateEnemies [] g         = return (g & enemies .~ [])
updateEnemies (e: rest) g  = do
                              randomNum <- (drawDouble 0 1)
                              restGame <- updateEnemies rest g 
                              if (randomNum < 0.25) then return (restGame & enemies %~ (++ [(moveDir g North e)]) )
                              else if (randomNum < 0.5) then return (restGame & enemies %~ (++ [(moveDir g South e)]) )
                              else if (randomNum < 0.75) then return (restGame & enemies %~ (++ [(moveDir g East e)])) 
                              else return (restGame & enemies %~ (++ [(moveDir g West e)]) )

killEnemies :: Game -> [Coord] -> Game
killEnemies g []        = g & enemies .~ []
killEnemies g (e: rest) = if (checkExplosion g e)
                           then restGame & score %~ (\x -> (x+scorePerEnemy))
                           else restGame & enemies %~ (++ [e])
  where
    restGame = killEnemies g rest


--Creating target for bomberman
getTarget :: IO Coord
getTarget = do
            let possibleCoord = getCoordWithoutWalls allCoords
            a <- drawInt 0 ((length possibleCoord)-1)
            return (possibleCoord !! a)
            where
            allCoords = [(V2 x y) | x <- [0..width-1], y <- [0..height-1]]


getCoordWithoutWalls :: [Coord] -> [Coord]
getCoordWithoutWalls [] = []
getCoordWithoutWalls (c: rest) = if checkWall c then restlist
                                else c : restlist
                                where restlist = getCoordWithoutWalls rest
                               
drawInt :: Int -> Int -> IO Int
drawInt x y = getStdRandom (randomR (x,y))


-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  let walls = getWalls
  brickwalls <- getBricks
  enemies <- getEnemy brickwalls
  target <- getTarget
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _bomberman  = (V2 1 1)
        , _walls  = walls
        , _brickwalls = brickwalls
        , _explosions = []
        , _bombs = []
        , _score  = 0
        , _dead   = False
        , _locked = False
        ,_enemies = enemies
        ,_target = target
        }
  return $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
