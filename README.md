# cse230_group32

<img width="450" alt="bomberman_project" src="https://user-images.githubusercontent.com/30871945/200434472-6ef756be-79dc-43d2-9daf-0a35cc2cf91e.png">

## Proposal:
Our goal is to create the bomberman game using Haskell. This is a one-player game where the player must reach the target from the initial position. There are 6 main components in the game called Player, Enemy, Brick, Wall, Target and Bomb.

The player must either avoid or destroy the enemies/bricks in the way. The player can destroy these obstacles by strategically planting bombs which will explode after a certain amount of time. The bomb will have an affect on the components in four directions top, bottom, left and right. Whenever an obstacle is destroyed the player is awarded points. The goal of the player is to reach the target with the highest score possible.

The enemies move randomly and will kill the player when both the enemy and the player occupy the same cell. Since it is harder to kill the enemy, the player will be awarded 10 points for killing it. When the player destroys the bricks they will be awarded 2 points.

The player will initially have 3 lives and each time the player is killed by the enemy (or) is a part of an explosion they will lose 1 life.

Link to the online game: https://www.retrogames.cz/play_085-NES.php?language=EN

## Goals:
- Our initial goal is to create a working version of the game having the features discussed above. This will allow the player to play one level.
- If time permits we want to add some of the following features:
    - Player can collect powerups using which they can move faster, deploy powerful bombs, get more lives etc.
    - There will be more levels with increasing difficulty.

