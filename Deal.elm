module Deal exposing (..)

import Random

type Suit = Spade | Heart | Diamond | Club
type alias Card = {rank : Int,
                   suit : Suit}
type alias Hand = List Card
type alias Deal = List Hand

-- creates a shuffled deck of integers
generateIntDeal : Int -> List Int -> Random.Seed -> List Int
generateIntDeal counter currentIntDeal lastSeed =
  let (randx, nextSeed) = Random.step (Random.int 0 counter) lastSeed in
  case counter of
    52 -> currentIntDeal
    _  -> generateIntDeal (counter + 1) (insertInt counter randx currentIntDeal) nextSeed

-- insert given number at given index of given list
insertInt : Int -> Int -> List Int -> List Int
insertInt n idx ns =
  case ns of
    [] -> [n]
    _ ->  (List.take idx ns) ++ [n] ++ (List.drop idx ns)

-- initiates the shuffling process
shuffle : Int -> List Int
shuffle seed =
  generateIntDeal 0 [] (Random.initialSeed seed)

separate : List Int -> List (List Int)
separate ns =
  case ns of
    [] -> []
    _ -> (List.take 13 ns) :: separate (List.drop 13 ns)

convertToCard : Int -> Card
convertToCard n =
  let suitx =
        if n < 13 then Club
        else if n < 26 then Diamond
        else if n < 39 then Heart
        else Spade
      rankx = modBy 13 n in
  {rank = rankx, suit = suitx}

handRecords : List (List Int) -> List (List Card)
handRecords intHands =
  case intHands of
    [] -> []
    head::tail -> (List.map convertToCard head) :: (handRecords tail)

convertCardToString : Card -> String
convertCardToString card =
  let stringRank =
        if card.rank < 9 then String.fromInt (card.rank + 2)
        else if card.rank == 9 then "J"
        else if card.rank == 10 then "Q"
        else if card.rank == 11 then "K"
        else "A"
      stringSuit =
        if card.suit == Spade then "S"
        else if card.suit == Heart then "H"
        else if card.suit == Diamond then "D"
        else "C" in
  stringSuit ++ stringRank

handStrings : List (List Card) -> List (List String)
handStrings cardHands =
  case cardHands of
    [] -> []
    head::tail -> (List.map convertCardToString head) :: (handStrings tail)