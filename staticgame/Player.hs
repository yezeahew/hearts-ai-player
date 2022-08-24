-- | Write a report describing your design and strategy here.
-- |=================================================================================================================================
-- | ---------IMPORTANT DEFINITIONS USED---------
-- | 1. win the trick - refers to aiming to take the trick  
-- |    (to take small amount of point, avoid others from shooting the moon)
-- |
-- | 2. lose the trick - refers to avoiding taking the trick 
-- |    (to avoid taking lots of point cards / SQ)
-- |
-- | 3. trick - refers to the cards played in a turn
-- | 4. leading suit - refers to the suit of the lead card 
-- | 5. largest lead card - refers to the largest card in current trick that follows the leading suit
-- | 
-- | ---------MEMORY---------
-- | The memory stores 3 things
-- |    1. char "H" if heart is broken 
-- |    2. the number of players in the game
-- |    3. "SQ" if Spade Queen has been used
-- |
-- | ---------OVERALL STRATEGY---------
-- | The overall strategy can be broken down into few parts leading , reneging
-- |
-- | ~ LEADING ~
-- | There are two scenarios : 
-- | Player either start a new game / lead a new turn
-- |    
-- |           Starting Strategy : Always try to play with Club Two.
-- |
-- |           Leading Strategy
-- |                  1. If Spade Queen is used, then lead with the largest spades that i have 
-- |
-- |                  2. else, if Spade Queen has not been played, NEVER lead with (SA,SQ,SK)    
-- |                       a. If heart is broken, lead with the smallest card that player has,can be hearts (excluding SA,SQ,SK)
-- |                       b. if heart is not broken, then try to lead with the smallest nonPoint cards (not Hearts / SQ)  
-- |                          if player only has point cards, then lead with SQ. (cannot lead with heart cause heart is not broken)
-- |
-- |                  3.Otherwise , if none of the above is applicable , just lead with the smallest ranking card.
-- |
-- | ~ RENEGING ~
-- | There are several different scenarios when reneging :
-- |
-- |           1. if player is playing last this round 
-- |                a. if current trick contains SQ/ alot of heart cards, apply LOSE strategy 
-- |                   player will try to play in a way such that he WILL NOT win the trick. 
-- |
-- |                b. if current trick has low points (<=2) , apply WIN strategy 
-- |                   player will play in a way such that he will WIN the trick.
-- |                   this is a great chance to discard player's high ranking cards.
-- |
-- |           2. if player is not the last player, just play normally.  (NORMAL strategy)
-- |
-- | ---------THE DIFFERENT STRATEGIES---------
-- | 
-- | ========================= NORMAL STRATEGY ========================= 
-- | When to use : when player is playing second / third in a 4 player game
-- | Main Goal : not win the trick
-- |
-- | 1. If player has cards that matches the leading suit,
-- |
-- |    a. If player has cards that is smaller than the largest leading suit card in the trick,
-- |        play the biggest possible card that is smaller than the largest leading suit.
-- |        EG            Player's Hand : [SA,SQ,D8,D10,C5,C9,CJ,CA,HA,HQ]
-- |                      Largest Suit Card in current trick : CK
-- |                      Player will discard : CJ
-- |
-- |    b. Player does not have cards that is smaller than the largest leading suit card in the trick,
-- |        then play the smallest card that matches the leading suit.
-- |        EG            Player's Hand : [SA,SQ,D8,D10,CJ,CA,HA,HQ]
-- |                      Largest Suit Card in current trick : D7
-- |                      Player will discard : D8
-- |
-- | 2. Player does not have cards that matches the leading suit
-- |
-- |    a. If its the first round of a new game, play the biggest nonPoint card (Bleeding Rule)
-- |    b. If its not the first round , play the biggest point card
-- |
-- | 3. If none of the above is applicable, just play the biggest card.
-- |
-- | ========================= WIN STRATEGY ========================= 
-- | When to use : 
-- |    i. player is playing LAST and the winning the trick only wins 0-2 points
-- |    ii. player is playing LAST, and someone played SPADE ACE/KING
-- | Main Goal : win the trick / discard spade queen
-- |
-- | 1. If other player played SPADE KING/ACE and player has SPADE QUEEN, discard SPADE QUEEN
-- |
-- | 2a. Player tries to play the maximum lead suit card , that is not SPADE QUEEN.
-- |     This is because if player plays the SPADE QUEEN card, he will end up winning 13 points. 
-- |
-- |     ** have to make sure length leadSuitCards > 1 to prevent the prelude.emptylist error
-- |
-- | 2b. ADDED TO TACKLE EDGE CASE: 
-- |     If the lead suit is SPADE, and player only has SPADE QUEEN and no other SPADE cards
-- |     then player will have to play the spade card and win the trick.
-- |
-- | 3. Player does not have cards that matches the leading suit, player is unable to win the current trick
-- |    Therefore, player will play the biggest point card
-- |
-- | 4. If none of the above is applicable, just play the biggest card.
-- |
-- | ========================= LOSE STRATEGY ========================= 
-- | When to use : player is playing LAST and the current trick has SQ/ more than 2 heart cards.
-- | Main Goal : not win the trick, avoid winning high points
-- |
-- | 1. If player has cards that matches the leading suit,
-- |
-- |     (SEE LINE 54 FOR EXAMPLE)
-- |     a. If player has cards that is smaller than the largest leading suit card in the trick,
-- |        play the biggest possible card that is smaller than the largest leading suit. 
-- |
-- |     b. Player does not have cards that is smaller than the largest leading suit card in the trick.
-- |        Player will have to take the trick regardless of what card he plays.
-- |        Thus, Player will play the BIGGEST card that matches the leading suit.
-- |        (Great way to discard high ranking cards)
-- |
-- | 2. Player does not have cards that matches the leading suit, play the biggest point card
-- |
-- | 3. If none of the above is applicable, just play the biggest card.
-- |
-- | ---------THE OVERALL DESIGN ---------
-- | The overall game design is described as below
-- |
-- | CARDS PLAYING SECTION
-- | => usage of guards to differentiate various conditions, this improves the readability of code and makes it easier to debug
-- | => when a certain condition is fullied, it will call one of the (3) STRATEGY functions to perform selections of cards
-- |
-- | GAME STRATEGY SECTION
-- | => logics of card filtering happens here
-- | => usage of guards to differentiate various conditions, this improves the readability of code and makes it easier to debug
-- | => selection of cards happens here
-- |
-- | HELPER FUNCTION SECTION
-- | => small reusable functions are placed here
-- | => these small reusable functions are mostly generalized to work under various scenarios
-- | => these functions are safe because it always returns a clean result without side effects
-- | => usage of small reusable functions offers referential transparency
-- | => these functions also obeys the singple responsibility principle as they only perform one tasks
-- |
-- | MEMORY MANIPULATION SECTION
-- | => this is where the memory is read / updated
-- |
-- |=================================================================================================================================

module Player (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Char
import Control.Monad
import Data.Maybe
import Data.List
import Hearts.Types

-- |===========================================================
-- |   
-- |                   CARDS PLAYING SECTION
-- |    
-- |===========================================================

playCard :: PlayFunc
-- Empty trick list means beginning of trick. I either have to start the game / lead a round .
-- if memory is Nothing, this indicates that its the first round of a new game, call the start function to start
-- otherwise , call lead function to lead a new round
playCard _ hand [] memory
    | memory == Nothing = (start hand, save memory)
    | otherwise = (lead hand memo, save memory)
    where 
        memo = retreiveMemory memory --to retreive the memory

-- Trick list is not empty, so it means someone already played. Call renege to follow the lead suit.
playCard _ hand trick memory 
    | length memo == 0 = (renege trick "" hand, save memory) --if its the first trick of a new game , memory is ""
    | otherwise = (renege trick memo hand, save memory)
    where 
        memo = retreiveMemory memory

--Renege: to follow the leading suit 
--takes in current trick, memo, and hand
--EXPLANATION SEE LINE 33
renege :: [(Card,PlayerId)] -> String -> [Card] -> Card
renege trick memo hand
    | length trick == player_num - 1 && highPoint = playLoseStrgy largest_card hand --line 37
    | length trick == player_num - 1 && not highPoint = playWinStrgy largest_card hand --line 40
    | otherwise = playStrgy largest_card hand memo --line 44
    where
        player_num = numPlayer memo         --int, number of players in the game  
        highPoint = highTrickPoint $ fst <$> trick --boolean , true if current trick contains SQ / more than 2 hearts
        lead_suit  = suit $ fst $ last trick --suit, the suit of the leading card
        largest_card = maximum $ filter ((lead_suit ==) . suit) $ fst <$> trick --card, the largest card in the trick that is under the leading suit

-- The start function is to play the first trick of the game 
-- Always try to play with Club Two
-- if player don't have Club Two, then play any non point cards. (Bleeding Rule)
start :: [Card] -> Card
start hand = select (find (Card Club Two ==) hand)
    where
    select :: Maybe Card -> Card
    select Nothing = minimum $ nonPoint hand
    select card = fromJust card

-- The lead function is for the start of the trick. 
-- This function selects an appropiate card to start a trick
-- Player's Strategy in leading a trick : SEE LINE 23
lead :: [Card] -> String -> Card
lead hand memory
    | sqUsed && length spades > 0 = maximum spades --LINE 24
    | heartbroken && length nonBigS > 0 = head $ sortCard nonBigS --LINE 27
    | not heartbroken && length npCards > 3 = head $ sortCard $ nonBigSpades npCards --LINE 28
    | not heartbroken && length nhCards > 0 = head $ sortCard $ nhCards --LINE 28
    | otherwise = head $ sortCard hand --LINE 31
    where 
        spades = filterSuit Spade hand --[Card] ,  a list of all the spade cards that player has
        nonBigS = nonBigSpades hand -- [Card] , a list of cards that player has excluding SQ,SK,SA
        sqUsed = checkSpadeQueen memory == True -- bool, To check if SQ has been played
        heartbroken = checkHeartBroken memory == True --bool, to check if heart is broken
        npCards = nonPoint hand --[Card] ,  a list of all the non point cards that player has
        nhCards = nonHeart hand --[Card] ,  a list of all the non heart cards that player has


-- |===========================================================
-- |   
-- |                   GAME STRATEGY SECTION
-- |    
-- |===========================================================



--play strategy is the Normal Strategy , it's called when player is playing second / third in a 4 player game
--Player's Normal strategy : for detail explanation see LINE 48
playStrgy :: Card -> [Card] -> String -> Card
playStrgy largeLeadCard hand memo
    | length lessThanLeadCard > 0 = maximum lessThanLeadCard -- LINE 54
    | length leadSuitCards > 0 = minimum leadSuitCards -- LINE 60
    | memo == "" && length npCards > 0 = maximum npCards -- LINE 68
    | length pCards > 0 = maximum pCards --LINE 69
    | otherwise = maximum hand -- LINE 71
    where
        leadSuitCards = filterSuit (suit largeLeadCard) hand --[Card], the cards that matches lead suit
        lessThanLeadCard = filterSmaller largeLeadCard leadSuitCards --[Card], the cards that matches lead suit && is smaller than the largest lead suit card
        npCards = nonPoint hand --[Card], all the nonPoint cards that player has
        pCards = pointCards hand --[Card],all the pointCards that player has


-- Lose Trick Strategy, to avoid winning points
-- Selects a card arrocding to the lose trick strategy
-- Lose strategy : for detail explanation see LINE 95
playLoseStrgy :: Card -> [Card] -> Card
playLoseStrgy largeLeadCard hand 
    | length lessThanLeadCard > 0 = maximum lessThanLeadCard -- LINE 102
    | length leadSuitCards > 0 = maximum leadSuitCards -- LINE 105
    | length pCards > 0 = maximum pCards --LINE 110
    | otherwise = maximum hand -- LINE 112
    where
        lessThanLeadCard = filterSmaller largeLeadCard leadSuitCards --[Card], the cards that matches lead suit && is smaller than the largest lead suit card
        leadSuitCards = filterSuit (suit largeLeadCard) hand --[Card], the cards that matches lead suit
        pCards = pointCards hand --[Card],all the pointCards that player has

-- WIN Trick Strategy, to win the trick / to discard spade queen
-- Selects a card arrocding to the win trick strategy
-- WIN strategy : for detail explanation see LINE 73
playWinStrgy :: Card -> [Card] -> Card
playWinStrgy largeLeadCard hand 
    | bigSpade && length spade_queen > 0 = maximum spade_queen -- LINE 79
    | length leadSuitCards > 1 = maximum $ filter (Card Spade Queen /=) leadSuitCards -- LINE 81 (more explaination on line 137)
    | length leadSuitCards == 1 = head leadSuitCards -- LINE 86
    | length npCards > 0 = maximum npCards -- LINE 90
    | otherwise = maximum hand -- LINE 93
    where
        bigSpade = largeLeadCard == Card Spade King || largeLeadCard == Card Spade Ace -- bool, to check if the lagest lead card is SK/SA
        spade_queen = spadeQueen hand -- [Card], to check if player has SQ
        leadSuitCards = filterSuit (suit largeLeadCard) hand --[Card], the cards that matches lead suit
        npCards = nonPoint hand --[Card], all the non point cards that player has



-- |===========================================================
-- |   
-- |                  HELPER FUNCTION SECTION
-- |    
-- |===========================================================        

--To genarate a list of cards that is under a certain suit
-- EG : filterSuit Club [(Card Spade Ace),(Card Diamond Ten),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Heart Five),(Card Spade Queen)] 
-- returns [CJ,CQ]        
filterSuit :: Suit -> [Card] -> [Card]
filterSuit = filter . (. suit) . (==)

--To genarate a list of cards that is smaller than a certain card 
-- EG : filterSmaller (Card Diamond King) [(Card Spade Ace),(Card Diamond Ten),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Heart Five),(Card Spade Queen)] 
-- returns [SA,D10,CJ,CQ,SQ]
filterSmaller :: Card -> [Card] -> [Card]
filterSmaller = filter . flip (<)

-- Note : I can use filterSuit and filterSmaller together to find a list of cards that is smaller than the leading card.
-- EG : Player's Hand : [SA,SQ,D8,D10,C5,C9,CJ,CA,HA,HQ]
--      Largest Suit Card in current trick : CK
--      
-- By doing :
--      leadSuitCards = filterSuit Club hand
--      lessThanLeadCard = filterSmaller CK leadSuitCards
--      lessThanLeadCard now consists of [C5,C9,CJ]
--      calling 'maximum lessThanLeadCard' returns CJ
-- This allows the player to follow the suit with the biggest possible card without winning the trick.


-- To retreive the actual memory string from Maybe Memory
retreiveMemory :: Maybe ([(Card,PlayerId)],String) -> String
retreiveMemory Nothing = ""
retreiveMemory (Just(_,memo)) = memo

--Returns true when current trick contains the Spade Queen / more than 2 heart cards 
highTrickPoint :: [Card] -> Bool
highTrickPoint trick = sq == 1 || h > 2
        where
            sq = length $ spadeQueen trick 
            h = length $ filterSuit Heart trick

-- nonPoint : to generate a list of cards that does not contains point cards
-- When its the player's turn to lead or start a game , player always try to lead with a nonPoint card.
-- EG : nonPoint [(Card Spade Ace),(Card Diamond Ten),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Heart Five),(Card Spade Queen)]
-- returns [SA,D10,CJ,CQ]
nonPoint :: [Card] -> [Card]
nonPoint = filter ((Heart /=) . suit) . filter (Card Spade Queen /=)

-- pointCards : to generate a list of cards that consist of only point cards
-- Play the point cards when player cannot follow a suit. 
-- EG : pointCards [(Card Spade Ace),(Card Diamond Ten),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Heart Five),(Card Spade Queen)]
-- returns : [H2,H5,SQ]
pointCards :: [Card] -> [Card]
pointCards = ap ((++) . filter ((Heart ==) . suit)) spadeQueen

-- to remove the high ranking spade cards (SQ, SK, SA)
-- Leading / Following a trick with those cards can result in winning 13 points.  
-- Our goal is to score as low points as possible. Removing the higher ranking spade cards allows the player
-- to discard other cards first, thereby increasing the chance of scoring the lowest.
-- EG : nonBigSpades [(Card Spade Ace),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Spade King),(Card Spade Queen)]
-- returns :[CJ,CQ,H2]
nonBigSpades :: [Card] -> [Card]
nonBigSpades = filter (liftM2 (&&) (Card Spade Queen /=) (liftM2 (&&) (Card Spade Ace /=) (Card Spade King /=)))

-- To filter out the spade queen card 
-- EG: spadeQueen [(Card Spade Ace),(Card Diamond Ten),(Card Heart Five),(Card Spade Queen)]
-- returns : [SQ]
spadeQueen :: [Card] -> [Card]
spadeQueen = filter (Card Spade Queen ==)

-- Genarates a list of nonHeart Cards
-- EG : nonHeart [(Card Spade Ace),(Card Club Jack),(Card Club Queen),(Card Heart Two),(Card Heart Five),(Card Spade King)]
-- returns : [SA,CJ,CQ,SK]
nonHeart :: [Card] -> [Card]
nonHeart = filter ((Heart /=) . suit)

-- suit : Given a card , return the suit of the card
suit :: Card -> Suit
suit (Card s _) = s

-- returns the number of players that is in the game
-- Number of players is set to 4 by default
-- isDigit filters out numbers from the memory string
-- "H2SQ" returns 2
-- "H4" returns 4
numPlayer :: String -> Int
numPlayer "" = 4
numPlayer memo = read (filter isDigit memo) :: Int 

-- Given a card ,return the rank of the card
-- use to sort the cards by rank 
rank :: Card -> Rank 
rank (Card _ r) = r

-- A function that sorts the card by rank 
-- [D2,SQ,H3,C9]
-- the original program sorts by suit in default, calling " minimum [SQ,C9,D2,H3] " returns SQ
--              sort by rank : [D2,H3,C9,SQ]
--              sort by suit : [SQ,C9,D2,H3]
-- I want it to retun D2, Thus, I have added my own sortCart function to sort the cards by rank
-- This allows me to always lead with the smallest ranking card that I have, thereby reducing the
-- probability of winning a trick.
sortCard :: [Card] -> [Card]
sortCard = sortOn rank


-- |===========================================================
-- |   
-- |                  MEMORY MANIPULATING SECTION
-- |    
-- |===========================================================   

-- to check if Heart is broken
-- returns True if a Heart Card has been played
--         otherwise False
checkHeartBroken :: String -> Bool
checkHeartBroken = isInfixOf "H"


-- to check if SQ is played
-- returns True when SQ is been played
--         otherwise False
checkSpadeQueen :: String -> Bool
checkSpadeQueen = isInfixOf "SQ"

-- to update memory
-- the memory stores 3 things
-- 1. char "H" if heart is broken 
-- 2. the number of players in the game
-- 3. "SQ" if Spade Queen has been used

-- EG : if the memory stores 
-- "H2SQ" - this indicates that heart is broken , its a 2 player game and the card Spade Queen has been played
-- "4" - this indicates that heart is NOT broken , its a 4 player game and the card Spade Queen hasn't been played
save :: Maybe ([(Card,PlayerId)],String) -> String
save Nothing = ""
save (Just(trick,memo))
    | brokenHeart && sqUsed = "H" ++ (show $ length trick) ++ "SQ"
    | not brokenHeart && sqUsed = show (length trick) ++ "SQ"
    | brokenHeart && not sqUsed = "H" ++ (show $ length trick)
    | otherwise = show $ length trick
    where
        -- converts trick into string of cards
        --EG : [((Card Spade Queen), "12"),((Card Diamond Two),"2")]
        -- becomes : "SQ D2 "

        -- this allows me to use isInfixOf 'H'/'SQ' to check if Heart /Spade Queen has been played
        stringTrick = (foldl(++) "" $ map(\(Card s r,_) -> show s ++ show r ++ " ")(trick)) 
        brokenHeart = checkHeartBroken memo || checkHeartBroken stringTrick
        sqUsed = checkSpadeQueen memo || checkSpadeQueen stringTrick


-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
