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
            case getNextBids system firstBid [] of
                Just (higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case defineBid foundBid.subsequentBids rest bidMeaning of
                    Just subSystem -> Just (higherPriorityBids ++ [BidDefinition {foundBid | subsequentBids = subSystem}] ++ lowerPriorityBids)
                        
                    Nothing -> Nothing
                         
                Nothing -> Nothing

-- Like defineBid, but takes a function that takes the old requirements and turns them into new requirements. Retains previous priority.
modifyBid : BiddingRules -> BidSequence -> (List HandRange -> Maybe (List HandRange)) -> Maybe BiddingRules
modifyBid system history bidModification =
    case history of
        []               -> Nothing
        lastBid::[]      -> subSystemModify system lastBid bidModification
        firstBid::rest -> 
            case getNextBids system firstBid [] of
                Just (higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case modifyBid foundBid.subsequentBids rest bidModification of
                    Just subSystem -> Just (higherPriorityBids ++ [BidDefinition {foundBid | subsequentBids = subSystem}] ++ lowerPriorityBids)
                        
                    Nothing -> Nothing
                         
                Nothing -> Nothing

--Helper function for modifyBid
subSystemModify : BiddingRules -> Bid -> (List HandRange -> Maybe (List HandRange)) -> Maybe BiddingRules
subSystemModify subSystem bid bidModification =
    case getNextBids subSystem bid [] of
        Nothing -> Nothing
        Just(higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case bidModification foundBid.requirements of
            Nothing -> Nothing
                
            Just newMeaning -> Just (higherPriorityBids ++ [BidDefinition {foundBid | requirements = newMeaning}] ++ lowerPriorityBids)
            
                 
-- Input: a system, a history (with last bid being the one we want to prioritize)
-- Output: a system, if the input history exists
prioritizeBid : BiddingRules -> BidSequence -> Maybe BiddingRules
prioritizeBid system history =
    case history of
        [] -> Nothing
        lastBid::[] -> case getNextBids system lastBid [] of
            Just (higherPriorityBids, foundBid, lowerPriorityBids) -> Just (List.reverse (List.drop 1 (List.reverse higherPriorityBids)) ++ [foundBid] ++ List.reverse (List.take 1 (List.reverse higherPriorityBids)) ++ lowerPriorityBids)
            Nothing -> Nothing 
        firstBid::rest ->
            case getNextBids system firstBid [] of
                Just (higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case prioritizeBid foundBid.subsequentBids rest of
                    Just subSystem -> Just (higherPriorityBids ++ [BidDefinition {foundBid | subsequentBids = subSystem}] ++ lowerPriorityBids)
                    Nothing -> Nothing
                Nothing -> Nothing
                    

--Input: a system, a history (with last bid being the one we want to deprioritize)
--Output: a system, if the input history exists
deprioritizeBid : BiddingRules -> BidSequence -> Maybe BiddingRules
deprioritizeBid system history = 
    case history of
        [] -> Nothing
        lastBid::[] -> case getNextBids system lastBid [] of
            Just (higherPriorityBids, foundBid, lowerPriorityBids) -> Just (higherPriorityBids ++ List.take 1 lowerPriorityBids ++ [foundBid] ++ List.drop 1 lowerPriorityBids)
            Nothing -> Nothing
        firstBid::rest ->
            case getNextBids system firstBid [] of
                Just (higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case deprioritizeBid foundBid.subsequentBids rest of
                    Just subSystem -> Just (higherPriorityBids ++ [BidDefinition {foundBid | subsequentBids = subSystem}] ++ lowerPriorityBids)
                    Nothing -> Nothing
                Nothing -> Nothing
                    

-- Input: a system, a history (with last bid being the one we want to remove)
-- Output: the system, minus the last bid, if the input history exists
removeBid : BiddingRules -> BidSequence -> Maybe BiddingRules
removeBid system history =
    case history of
        [] -> Nothing
        lastBid::[] -> case getNextBids system lastBid [] of
            Just (higherPriorityBids, _, lowerPriorityBids) -> Just (higherPriorityBids ++ lowerPriorityBids)
            Nothing -> Nothing
        firstBid::rest ->
            case getNextBids system firstBid [] of
                Just (higherPriorityBids, BidDefinition foundBid, lowerPriorityBids) -> case removeBid foundBid.subsequentBids rest of
                    Just subSystem -> Just (higherPriorityBids ++ [BidDefinition {foundBid | subsequentBids = subSystem}] ++ lowerPriorityBids)
                    Nothing -> Nothing
                Nothing -> Nothing
                    
            


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
getNextBids : BiddingRules -> Bid -> List BidDefinition -> Maybe (List BidDefinition, BidDefinition, List BidDefinition)
getNextBids bidList bid previousBids =
    case bidList of
        [] -> Nothing
        (priorityBid)::rest -> let (BidDefinition unwrappedPriorityBid) = priorityBid in   
                                if bid == unwrappedPriorityBid.bidValue
                                then Just (previousBids, priorityBid, rest)
                                else getNextBids rest bid (priorityBid::previousBids)

getBid : BiddingRules -> List Bid -> Maybe BidDefinition
getBid system history =
    case history of
        [] -> Nothing
        lastBid::[] -> case getNextBids system lastBid [] of
            Nothing -> Nothing 
            Just (_, foundBid, _) -> Just foundBid
        firstBid::rest -> case getNextBids system firstBid [] of
            Nothing -> Nothing
            Just (_, BidDefinition foundBid, _) -> getBid foundBid.subsequentBids rest
                 
            

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

--Input: a system, a sequence of bidding
--Output: the system following that sequence, if it is defined
traverseSystem : Maybe BiddingRules -> BidSequence -> Maybe BiddingRules
traverseSystem maybeSystem history =
 case (maybeSystem, history) of
    (Nothing, _) -> Nothing
    (Just system, []) -> Just system    
    (Just system, firstBid::rest) -> case getNextBids system firstBid [] of
                Just (_, BidDefinition foundBid, _) -> traverseSystem (Just foundBid.subsequentBids) rest
                Nothing -> Nothing

--Input: A bid
--Output: A string denoting that bid
bidToString : Bid -> String
bidToString (number, suit) =
 case suit of
    Spade -> String.fromInt number ++ "♠"
    Heart -> String.fromInt number ++ "♥"
    Diamond -> String.fromInt number ++ "♦"
    Club -> String.fromInt number ++ "♣"
    NoTrump -> String.fromInt number ++ "NT"
    Pass -> "Pass"

--Inverse of the above function
stringToBid : String -> Maybe Bid
stringToBid string =
    if string == "Pass" || string == "pass" then Just (0, Pass)
    else case (String.toInt (String.left 1 string)) of
        Just int ->
         case (String.dropLeft 1 string) of
             "S" -> Just (int, Spade)
             "H" -> Just (int, Heart)
             "D" -> Just (int, Diamond)
             "C" -> Just (int, Club)
             "N" -> Just (int, NoTrump)
             "NT" -> Just (int, NoTrump)
             _ -> Nothing
        Nothing -> Nothing

--Input: A string of bids of the form "1C-1H-1N-2N"
--Output: A BidSequence
stringToSequence : String -> BidSequence
stringToSequence string = List.filterMap (stringToBid) (String.split "-" string)

--A HandRange consisting of all hands
allHands : HandRange
allHands = {spades = (0,13), hearts = (0,13), diamonds = (0,13), clubs = (0,13), points = (0,37)}

stringToType : String -> HandType -> Suit -> HandType
stringToType handString acc suit =
    let nextString = String.dropLeft 1 handString in
    case String.left 1 handString of
        ""  -> acc
        " " -> stringToType nextString acc suit
        "♠" -> stringToType nextString acc Spade
        "♥" -> stringToType nextString acc Heart
        "♦" -> stringToType nextString acc Diamond
        "♣" -> stringToType nextString acc Club
        "A" -> 
            let addedToSuit = addToSuit acc suit in
            {addedToSuit | points = acc.points + 4}
        "K" -> 
            let addedToSuit = addToSuit acc suit in
            {addedToSuit | points = acc.points + 3}
        "Q" -> 
            let addedToSuit = addToSuit acc suit in
            {addedToSuit | points = acc.points + 2}
        "J" -> 
            let addedToSuit = addToSuit acc suit in
            {addedToSuit | points = acc.points + 1}
        _ -> addToSuit acc suit

addToSuit : HandType -> Suit -> HandType
addToSuit acc suit =
    case suit of
        Spade -> {acc | spades = acc.spades + 1}
        Heart -> {acc | hearts = acc.hearts + 1}
        Diamond -> {acc | diamonds = acc.diamonds + 1}
        Club -> {acc | clubs = acc.clubs + 1}
        _ -> Debug.todo "Require for soundess"