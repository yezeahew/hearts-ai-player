-- | Write a report describing your design and strategy here.
module NaivePlayer (
    playCard,
    makeBid
)
where

-- You can add more imports as you need them.
import Cards
import Data.Maybe
import Data.List
import Hearts.Types


playCard :: PlayFunc
-- Empty trick list means beginning of trick. I have to lead.
playCard _ hand [] _ = (lead hand, "")
-- Trick list is not empty, so it means someone already played. follow the suit.
playCard _ hand trick _ = (follow (suit $ fst $ last trick) hand, "")

    

-- Chooses a card that follows the leading suit
follow :: Suit ->[Card] -> Card
follow lead_suit hand 
    -- | len_non_heart == empty_lead_s = head hand
    | length lead_s > 0 = head lead_s
    | length nonHeartCards > 0 = head nonHeartCards
    | otherwise = head hand
    where 
        lead_s = filter (\x -> suit x == lead_suit) hand
        nonHeartCards = nonHeart hand
    -- empty_lead_s = length lead_s == 0
    -- len_non_heart = length nonHeartCards


-- | The lead function is for the start of the trick. It always tries to choose 2 of clubs if I have it.
lead :: [Card] -> Card
lead hand = select (find (== (Card Club Two)) hand) 
    where
    -- | Select the first card from hand if no particular card was provided. Does not satisfiy Bleeding rule.
    select :: Maybe Card -> Card
    --select Nothing = head (nonPoint hand)
    select Nothing 
        | length nonPointCards > 0 = head nonPointCards
        | length nonHeartCards > 0 = head nonHeartCards
        | otherwise = head hand
        where 
            nonPointCards = nonPoint hand 
            nonHeartCards = nonHeart hand

    select card = fromJust card

-- nonPoint : to filter out the point cards
nonPoint :: [Card] -> [Card]
nonPoint cards = (filter (\x -> suit x /= Heart) . filter(/= (Card Spade Queen))) cards

-- nonHeart : to filter out the Hearts
nonHeart :: [Card] -> [Card]
nonHeart cards = (filter (\x -> suit x /= Heart)) cards

-- suit : Given a card , return the suit of the card
suit :: Card -> Suit
suit (Card s _) = s

-- | Not used, do not remove.
makeBid :: BidFunc
makeBid = undefined
