type alias Bid = (Int, Suit)
// a third thing
type Suit = Spade | Heart | Diamond | Club
type alias Points = Int
type alias Shape = {spades   : Int,
                    hearts   : Int,
                    diamonds : Int,
                    clubs    : Int}
type alias HandType = (Shape, Points)
type alias BidDefinition = {requirements   : List HandType,
                            bidValue       : Bid, 
                            subsequentBids : BiddingRules}
type alias BiddingRules = List BidDefinition
type alias BidSequence = List Bid

defineBid : BiddingRules -> BidSequence -> List HandType -> Bid -> BiddingRules
defineBid : 

removeBid : BiddingRules -> BidSequence -> BiddingRules


makeBid : BiddingRules -> HandType -> Maybe (Bid, BiddingRules)
makeBid : nextBids hand =
    case nextBids of
        []                  -> Nothing
        priorityBid::rest   ->  if correctBid priorityBid hand 
                                then Just (priorityBid.bidValue, priorityBid.subsequentBids)
                                else makeBid rest hand
            


correctBid : BidDefinition -> HandType -> Bool
correctBid bid hand = 
    case bid.requirements of
    []             -> False
    handType::rest ->   if handType == hand 
                        then True
                        else correctBid (rest, bid.bidValue, bid.subsequentBids) hand

