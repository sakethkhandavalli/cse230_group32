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

## Milestone-3 Updates:

### What is the architecture of your application (the key components)?
High-level architecture diagram:
![image](https://user-images.githubusercontent.com/30871945/203180194-26e3f48f-ec5a-46ca-98ec-9407ff41449d.png)

The key components in our game state are shown in the following diagram:
![image](https://user-images.githubusercontent.com/30871945/203180133-83c14a0a-ce25-46b0-ba22-4a987e846fe6.png)


### What challenges (if any) did you have so far and how did you solve them?
- Firstly we had some challenges getting familiar with the brick library, and the internal architecture that is used to render games. All team members were new to this, and thought it was challenging to get started
- In general programming languages like Java, Python it is very easy to get/set variables in objects. But in Haskell this gets a bit complicated, to tackle this challenge we learnt about Lenses. Lenses provide an easy way for accessing/updating fields in objects.
- Since Haskell is a pure language we need to mention IO side-effects even for generating random numbers. This was a bit counter-intuitive at start. Apart from that, we had to rewrite the functions with IO side-effects using do blocks.

### Do you expect to meet your goals before the deadline?
- We expect to meet our initial goal of creating a working version with the essential features of the game discussed in the README.
- If time permits we want to add some advanced features like power-ups and levels with increasing difficulty. 

### If not, how will you modify your goals?
- We do not intend to modify our goals.
