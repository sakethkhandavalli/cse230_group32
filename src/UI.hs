{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Bomberman

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Bomberman | Wall | BrickWall | Explosion | Bomb | Empty | Enemy | Target

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = liftIO (step g) >>= continue
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moveBomberman North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moveBomberman South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ moveBomberman East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ moveBomberman West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'b') [])) = continue $ plantBomb g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 200000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver g
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if (g ^. dead)
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
  else if (g ^. success)
     then withAttr successAttr $ C.hCenter $ str "YOU WON"
  else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Bomberman")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. walls               = Wall
      | c `elem` g ^. explosions          = Explosion
      | c `elem` g ^. brickwalls          = BrickWall
      | c `elem` (getBombLocs g)          = Bomb
      | c `elem` g ^. enemies             = Enemy
      | c == g ^. bomberman               = Bomberman
      | c == g ^. target                  = Target
      | otherwise                         = Empty

drawCell :: Cell -> Widget Name
drawCell Bomberman = withAttr bombermanAttr (str "   ")
drawCell Wall      = withAttr wallAttr (str "   ")
drawCell BrickWall = withAttr brickAttr (str "   ")
drawCell Bomb      = withAttr bombAttr (str "   ")
drawCell Explosion = withAttr explosionAttr (str "   ")
drawCell Empty     = withAttr emptyAttr (str "   ")
drawCell Enemy     = withAttr enemyAttr (str "   ")
drawCell Target    = withAttr targetAttr (str "   ")

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (bombermanAttr, V.blue `on` V.blue)
  , (wallAttr, V.black `on` V.black)
  , (brickAttr, V.yellow `on` V.yellow)
  , (bombAttr, V.red `on` V.red)
  , (explosionAttr, V.white `on` V.white)
  , (emptyAttr, V.green `on` V.green)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  , (enemyAttr, V.magenta `on` V.magenta)
  , (targetAttr, V.cyan `on` V.cyan)
  , (successAttr, fg V.green `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

bombermanAttr, wallAttr, emptyAttr :: AttrName
bombermanAttr = "bombermanAttr"
wallAttr      = "wallAttr"
brickAttr     = "brickAttr"
bombAttr      = "bombAttr"
explosionAttr = "explosionAttr"
emptyAttr     = "emptyAttr"
enemyAttr     = "enemyAttr"
successAttr   = "successAttr"
targetAttr    = "targetAttr"
