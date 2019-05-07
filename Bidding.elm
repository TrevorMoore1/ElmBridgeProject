module Bidding exposing (..)

type alias Bid = (Int, Suit)
type Suit = Spade | Heart | Diamond | Club | NoTrump | Pass
type alias HandRange = {spades   : (Int,Int),
                        hearts   : (Int,Int),
                        diamonds : (Int,Int),
                        clubs    : (Int,Int),
                        points   : (Int,Int)}
type alias HandType = { spades   :   Int,
                        hearts   :   Int,
                        diamonds :   Int,
                        clubs    :   Int,
                        points   :   Int}
type BidDefinition = BidDefinition
                            {requirements   : List HandRange,
                            bidValue       : Bid, 
                            subsequentBids : List BidDefinition}
type alias BiddingRules = List BidDefinition
type alias BidSequence = List Bid



-- Input: an entire system, a history (ending in bid that we're defining), applicable hand types
-- Output: a new bidding rule
defineBid : BiddingRules -> BidSequence -> List HandRange -> Maybe BiddingRules
defineBid system history bidMeaning =
    case history of
        []               -> Nothing
        lastBid::[]      -> Just ((BidDefinition{requirements = bidMeaning, bidValue = lastBid, subsequentBids = []})::system)
        firstBid::rest -> 
            case getNextBids system firstBid of
                Just subsequentBids -> defineBid subsequentBids rest bidMeaning
                Nothing -> Nothing
                
-- Input: a system, a history (with last bid being the one we want to prioritize)
-- Output: a system
prioritizeBid : BiddingRules -> BidSequence -> BiddingRules
prioritizeBid system previousBids =
    Debug.todo "TODO"

-- Input: a system, a history (with last bid being the one we want to remove)
-- Output: the system, minus the last bid
removeBid : BiddingRules -> BidSequence -> BiddingRules
removeBid system history =
    Debug.todo "TODO"


-- Input: a system, current handtype
-- Output: makes a bid if possible
makeBid : BiddingRules -> HandType -> Maybe (Bid, BiddingRules)
makeBid nextBids hand =
    case nextBids of
        []                  -> Nothing
        priorityBid::rest   ->  if correctBid priorityBid hand 
                                then 
                                    let (BidDefinition topChoice) = priorityBid in
                                    Just (topChoice.bidValue, topChoice.subsequentBids)
                                else makeBid rest hand
            
-- Input: a potential bid, the current hand
-- Output: whether the bid is applicable
correctBid : BidDefinition -> HandType -> Bool
correctBid (BidDefinition bid) hand = 
    List.foldr (||) False (List.map (handInRange hand) bid.requirements)

-- Input: a system, a bid
-- Output: 
getNextBids : BiddingRules -> Bid -> Maybe BiddingRules
getNextBids bidList bid =
    case bidList of
        [] -> Nothing
        (BidDefinition priorityBid)::rest ->    if bid == priorityBid.bidValue
                                then Just priorityBid.subsequentBids
                                else getNextBids rest bid

-- Input: current hand, candidate range
-- Output: whether the hand fits the range
handInRange : HandType -> HandRange -> Bool
handInRange hand range =
       hand.spades >= Tuple.first(range.spades)
    && hand.spades <= Tuple.second(range.spades)
    && hand.spades >= Tuple.first(range.hearts)
    && hand.spades <= Tuple.second(range.hearts)
    && hand.spades >= Tuple.first(range.diamonds)
    && hand.spades <= Tuple.second(range.diamonds)
    && hand.spades >= Tuple.first(range.clubs)
    && hand.spades <= Tuple.second(range.clubs)
    && hand.spades >= Tuple.first(range.points)
    && hand.spades <= Tuple.second(range.points)