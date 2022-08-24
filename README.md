# Functional Programming Assignment: Hearts
**Task : Implement an AI agent that plays a trick-based card game called Hearts.**    
- My implementation can be found in Player.hs  
- The logics / strategies I used is discussed in detail in the Player.hs file

## Assignment Specs 

The game of Hearts[^1] is a trick-taking game where the goal to score as *few
points* as possible.  All cards in the Hearts are worth one point, and the
Queen of Spade is worth 13.  Your task will be to implement a simple AI to beat
other players (students and staff) at this game.

There will be two versions of the game:

 1. A two-player version, which will be used to assess the *strength* of your
    player.  This version will use calibrated opponents and can be seen as a
    test.
 2. An online tournament where your player will be pitted against other AIs in
    four-player matches.

To play the game with your AI, you will need to implement a function called
`playFunc` which will be given a number of parameters upon which you will need
to act and return the card you believe will lead to victory.  An important
component of the game setup is that you will be allowed to have a form of
*memory*.

Memory, in this case, will be entirely determined by you.  The game will simply
pass you information pertaining to the current round: your hand and other cards
played; and the previous round: the complete set of cards played at the
previous round and your memory.

The game you will play will follow most of the classic rules of Hearts.  This
game is very popular as it is easy to lean and play, thus many online versions
are available if you want to train.  Do note, though, that most of them usually
vary a little, so take care to read the rules carefully.

At the core of the game of Hearts are the *point cards*: all cards in Hearts
and the Queen of Spade.  The goal of the game is to score as few points as
possible.  An alternative exists though: if a player manages to take all point
cards, she gets 0 points and *every other player* gets 26 points (the maximum).

At the end of each *round*, when players have played all the cards in their
hands, we tally the points in every trick taken by each player.  That is, we
sum the point cards they won.

The game goes on until at least one player has scored more
than 100 points *and* there is exactly one player with the lowest score.  The
player with the lowest score is the winner.

Counting points in Hearts is done as follows:

 1. **Point cards:** Each Hearts is worth 1 point, the Queen of Spades is worth
    13 points.
 2. **Hand:** At the end of each hand (players have played all their cards),
    each player receives the amount of points in the tricks they won.
 3. **Game:** The game stops when a player has reached 100 points and exactly
    one other has the lowest score; the latter is the winner.

The rules of Hearts we will use are as follows:

 1. **Reneging:** Each turn starts with a player playing a single card, the
    suit of which determines the suit of the trick.  Other players must *follow
    suit* (play cards in the same suit) if they are able to; if they do not
    have any cards in that suit, they may discard any card of their choosing.
 2. **Bleeding:** No point card may be played during the first round unless the
    player has no other choice.
 3. **Leading:** The player with the Two of Clubs leads (starts) the first
    round with this card.
 4. **Breaking:** No player may start a trick with a Heart before any has been
    played, unless they have no other choice.
 5. **Shooting the Moon:** In case a player took all the point cards, she
    scores 0 points and the other players all score 26 points.


