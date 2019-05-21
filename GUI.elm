module GUI exposing (main)

import Deal exposing (Card, shuffle, separate, handRecords, newDeal)
import Browser
import Bidding exposing (..)
import Html exposing (Html)
import Html.Events exposing (onClick, onInput)

-- MODEL

type Location = BidPractice | Loc BidSequence

type alias Model = {nextSeed : Int,
                    hands : List (List Card),
                    system : BiddingRules,
                    location : Location}

initModel = {nextSeed = 0,
             hands = [[],[],[],[]],
             system = [],
             --system = [BidDefinition {requirements = [], bidValue = (1,Heart), subsequentBids = []}],
             location = Loc []}


-- UPDATE

type Msg = Redeal | Goto Location | UpdateSystem (Maybe BiddingRules) | NoUpdate

update : Msg -> Model -> Model
update msg model = 
  let {nextSeed, hands} = model in
  case msg of
    Redeal ->
      let newHands = handRecords (separate (shuffle nextSeed))
-- this is NOT a good method
          newSeed = nextSeed + 1 in
      {nextSeed = newSeed, hands = newHands, system = model.system, location = model.location}
    Goto newLocation -> {model | location = newLocation}
    UpdateSystem maybeSystem -> case maybeSystem of
      Nothing -> model
        
      Just newSystem ->
         {model | system = newSystem}
    NoUpdate -> model 



-- VIEW

view : Model -> Html Msg
view model =
  case model.location of
    BidPractice ->
      let {nextSeed, hands} = model in
      let redeal = Html.button [onClick Redeal] [Html.text "Redeal"]
          display = Html.text (Deal.newDeal nextSeed) in
      Html.div [] [redeal, display]
    Loc bidSequence -> 
      let buttonList = Html.button [onClick (Goto (Loc []))] [Html.text "Begin"]::(makeButtonList [] bidSequence) in
      let searchBox = Html.textarea [onInput searchBoxFunction] [] in
      let nextBids = displayNextBids model.system bidSequence in
      Html.div [] [Html.p [] buttonList, Html.p [] [searchBox], Html.p [] nextBids]

 --Helper Functions

searchBoxFunction : String -> Msg
searchBoxFunction string =
  case String.right 1 string of
    "\n" -> (Goto (Loc (stringToSequence (String.dropRight 1 string))))
      
    _ -> NoUpdate
      
makeButtonList : BidSequence -> BidSequence -> List (Html Msg)
makeButtonList prevBids futureBids =
  case futureBids of
   [] -> [] 
   bid::rest -> Html.button [onClick (Goto (Loc (prevBids++[bid])))] [Html.text (bidToString bid)]::(makeButtonList (prevBids++[bid]) rest)

displayNextBids : BiddingRules -> BidSequence -> List (Html Msg)
displayNextBids system history =
  let newBox = [Html.textarea [onInput (newBidFunction system history)] [Html.text "Create New Bid"]]
  in case (traverseSystem (Just system) history) of
    Nothing -> newBox
    Just nextBids -> (List.map (displayBid system history) nextBids) ++ newBox

newBidFunction : BiddingRules -> BidSequence -> String -> Msg
newBidFunction system history string =
  case String.right 1 string of
    "\n" -> case stringToBid (String.dropRight 1 string) of
      Nothing -> NoUpdate
        
      Just bid -> (UpdateSystem (defineBid system (history++[bid]) []))
      
    _ -> NoUpdate
      
      

displayBid : BiddingRules -> BidSequence -> BidDefinition -> Html Msg
displayBid system history (BidDefinition bid) =
 let followingBids = Html.button [onClick (Goto (Loc history))] [Html.text (bidToString bid.bidValue)]
 in let prioritize = Html.button [onClick (UpdateSystem (prioritizeBid system history))] [Html.text ("<=")]
 in let deprioritize = Html.button [onClick (UpdateSystem (deprioritizeBid system history))] [Html.text ("=>")]
 in let modify = Html.button [] [Html.text "Modify"]
 in let delete = Html.button [onClick (UpdateSystem (removeBid system history))] [Html.text "Delete"]
 in Html.div [] [followingBids, prioritize, deprioritize, modify, delete]


-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox {init = initModel,
                   view = view,
                   update = update}