module Deal exposing (..)

import Random

type Suit = Spade | Heart | Diamond | Club
type alias Card = {rank : Int,
                   suit : Suit}
type alias Hand = List Card
type alias Deal = List Hand
type alias SeparatedHand = {spades : List Card,
                            hearts : List Card,
                            diamonds : List Card,
                            clubs : List Card}

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

-- takes a shuffled int deck and splits into 4 hands of 13
separate : List Int -> List (List Int)
separate ns =
  case ns of
    [] -> []
    _ -> (List.take 13 ns) :: separate (List.drop 13 ns)

-- converts an integer 0-51 into its suit and rank
convertToCard : Int -> Card
convertToCard n =
  let suitx =
        if n < 13 then Club
        else if n < 26 then Diamond
        else if n < 39 then Heart
        else Spade
      rankx = modBy 13 n in
  {rank = rankx, suit = suitx}

-- takes list of 4 int hands and converts to list of 4 card hands
handRecords : List (List Int) -> List (List Card)
handRecords intHands =
  case intHands of
    [] -> []
    head::tail -> (List.map convertToCard head) :: (handRecords tail)

cardSuitString : Card -> String
cardSuitString card =
  if card.suit == Spade then "S"
  else if card.suit == Heart then "H"
  else if card.suit == Diamond then "D"
  else "C"

cardRankString : Card -> String
cardRankString card =
  if card.rank < 9 then String.fromInt (card.rank + 2)
  else if card.rank == 9 then "J"
  else if card.rank == 10 then "Q"
  else if card.rank == 11 then "K"
  else "A"

-- takes a card and converts to its string representation
convertCardToString : Card -> String
convertCardToString card =
  (cardSuitString card) ++ (cardRankString card)

-- takes a list of 4 card hands and converts to list of 4 string hands
handStrings : List (List Card) -> List (List String)
handStrings cardHands =
  case cardHands of
    [] -> []
    head::tail -> (List.map convertCardToString head) :: (handStrings tail)

-- sorts a list of cards into correct suits
sortCardsBySuit : SeparatedHand -> List Card -> SeparatedHand
sortCardsBySuit init cards =
  let {spades, hearts, diamonds, clubs} = init in
  case cards of
    [] -> init
    c1::cs ->
      case c1.suit of
        Spade -> sortCardsBySuit {spades = c1::spades, hearts = hearts, diamonds = diamonds, clubs = clubs} cs
        Heart -> sortCardsBySuit {spades = spades, hearts = c1::hearts, diamonds = diamonds, clubs = clubs} cs
        Diamond -> sortCardsBySuit {spades = spades, hearts = hearts, diamonds = c1::diamonds, clubs = clubs} cs
        Club -> sortCardsBySuit {spades = spades, hearts = hearts, diamonds = diamonds, clubs = c1::clubs} cs

-- sorts a list of suit-sorted cards into numerical order
sortCardsByRank : SeparatedHand -> SeparatedHand
sortCardsByRank hand =
  let {spades, hearts, diamonds, clubs} = hand
      sorter = List.sortBy .rank in
  {spades = sorter spades, hearts = sorter hearts, diamonds = sorter diamonds, clubs = sorter clubs}


-- displays a hand more readably
displayHand : SeparatedHand -> String
displayHand hand =
  let {spades, hearts, diamonds, clubs} = hand in
  let spadesRankStrings = List.map cardRankString spades
      heartsRankStrings = List.map cardRankString hearts
      diamondsRankStrings = List.map cardRankString diamonds
      clubsRankStrings = List.map cardRankString clubs in
  let spadeString = List.foldl (++) "" spadesRankStrings
      heartString = List.foldl (++) "" heartsRankStrings
      diamondString = List.foldl (++) "" diamondsRankStrings
      clubString = List.foldl (++) "" clubsRankStrings in
  "S" ++ spadeString ++ " H" ++ heartString ++ " D" ++ diamondString ++ " C" ++ clubString ++ "\n"

-- controller function for printing a full deal, given a seed
newDeal : Int -> String
newDeal seed =
  List.foldl (++) ""
    (List.map displayHand
      (List.map sortCardsByRank
        (List.map (sortCardsBySuit {spades=[], hearts=[], diamonds=[], clubs=[]})
          (handRecords (separate (shuffle seed))))))