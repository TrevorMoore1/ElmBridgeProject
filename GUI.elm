module GUI exposing (main)

import Bidding exposing (..)
import Deal exposing (Card, shuffle, separate, handRecords, newDeal)
import Browser
import Bidding exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput)
import Task exposing (..)
import Time exposing (..)


-- MODEL

type Location = Practice | Edit

type alias Model = {nextSeed : Int,
                    passes : Int,
                    location : Location,
                    system : BiddingRules,
                    bidSequence : BidSequence,
                    debug : String}
-- Suited bids are #0-34, then X, XX, P

initModel = {   nextSeed = 0, 
                passes = 0,
                location = Practice,
                bidSequence = [],
                system = [],
                debug = "Debug"}


-- UPDATE

type Msg =
    RequestTime
  | ReceiveTime Posix
  | BidsMade BidSequence
  | ChangeBiddingSequence BidSequence
  | Goto Location
  | UpdateSystem (Maybe BiddingRules)
  | NoUpdate
  | DebugString String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestTime -> (model, Task.perform ReceiveTime Time.now)
    ReceiveTime time ->
        let {nextSeed} = model in
        let newSeed = Time.posixToMillis time in
        ({model | nextSeed = newSeed, passes = 0, bidSequence = []}, Cmd.none)
    BidsMade bids ->
        case bids of
            (level, Pass)::rest -> ({model | passes = model.passes + 1, bidSequence = model.bidSequence ++ bids}, Cmd.none)
            (level, _)::rest -> ({model | passes = 0, bidSequence = model.bidSequence ++ bids}, Cmd.none)
            _ -> (model, Cmd.none)
    ChangeBiddingSequence bids ->
        ({model | bidSequence = bids}, Cmd.none)
    --ChangeBiddingSequence overwrites the previous sequence, BidsMade adds to it
    Goto newLocation -> ({model | passes = 0, location = newLocation, bidSequence = []}, Cmd.none)
    UpdateSystem maybeSystem ->
        case maybeSystem of
            Nothing -> (model, Cmd.none)
            Just newSystem ->
                ({model | debug = String.fromInt(List.length(newSystem)), system = newSystem}, Cmd.none)
                --({model | debug = "Updated System"}, Cmd.none)
    NoUpdate -> (model, Cmd.none)
    DebugString string -> ({model | debug = string}, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    case model.location of
        Practice -> 
            let redeal = Html.button [onClick RequestTime] [Html.text "Redeal"] in
            let practice = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Practice"] in
            let edit = Html.button [onClick (Goto Edit)] [Html.text "Edit System"] in
            let availableBids = bidDisplay model in
            let handString = Deal.newDeal model.nextSeed in
            let monoStyle = Html.Attributes.style "font-family" "courier" in
            let bidsMadeButtons = Html.div [] (makeBidsMadeButtons model.bidSequence) in
            case handString of
                n :: e :: s :: w :: [] ->
                    Html.div [] ([  Html.div [] [practice, edit],
                                    redeal,
                                    Html.div [monoStyle] [Html.text n],
                                    Html.div [monoStyle] [Html.text e],
                                    Html.div [monoStyle] [Html.text s],
                                    Html.div [monoStyle] [Html.text w]]
                                    ++ availableBids
                                    ++ [bidsMadeButtons])
                _ -> Debug.todo "view failed"
        Edit ->
            let practice = Html.button [onClick (Goto Practice)] [Html.text "Practice"] in
            let edit = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Edit System"] in
            let buttonList = Html.button [onClick (ChangeBiddingSequence [])] [Html.text "Begin"]::(makeButtonList [] model.bidSequence) in
            let searchBox = Html.textarea [onInput searchBoxFunction] [] in
            let instructions = Html.p [] [Html.text "Create new bid"] in
            --let debugMessage = Html.p [] [Html.text model.debug] in
            --let debugMessage = Html.textarea [] [Html.text (getLowerPoint model.system [(1,Spade)])] in
            let newBidBox = [Html.textarea [onInput (newBidFunction model.system model.bidSequence)] []] in
            --let newBidBox = [Html.textarea [onInput DebugString] []] in
            let nextBids = displayNextBids model.system model.bidSequence in
            Html.div [] [   Html.div [] [practice, edit],
                            Html.div [] buttonList, 
                            Html.div [] [searchBox], 
                            instructions,
                            Html.div [] newBidBox,
                            Html.div [] nextBids
                            --debugMessage
                            ]

makeBidsMadeButtons : BidSequence -> List (Html Msg)
makeBidsMadeButtons bidSequence =
    case bidSequence of
        [] -> []
        bid :: rest ->
            (Html.button [] [Html.text (Bidding.bidToString bid)]) :: (makeBidsMadeButtons rest)


makeBidBoxButton : Bid -> Html Msg
makeBidBoxButton bid =
    Html.button [onClick (BidsMade [bid])] [Html.text (bidToString bid)]


makeBidBoxButtonInvis : Bid -> Html Msg
makeBidBoxButtonInvis bid =
    Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text (bidToString bid)]

attachBidBoxButtons : List (Bid, Bool) -> List (Html Msg)
attachBidBoxButtons bids =
    case bids of
        [] -> []
        (bid, allowed) :: rest ->
            if allowed then (makeBidBoxButton bid) :: attachBidBoxButtons rest
            else (makeBidBoxButtonInvis bid) :: attachBidBoxButtons rest 

bidDisplay : Model -> List (Html Msg)
bidDisplay model =
    let func = (\b -> (b, isBidAllowed b model)) in
    let level1 = attachBidBoxButtons (List.map func [(1,Club),(1,Diamond),(1,Heart),(1,Spade),(1,NoTrump)])
        level2 = attachBidBoxButtons (List.map func [(2,Club),(2,Diamond),(2,Heart),(2,Spade),(2,NoTrump)])
        level3 = attachBidBoxButtons (List.map func [(3,Club),(3,Diamond),(3,Heart),(3,Spade),(3,NoTrump)])
        level4 = attachBidBoxButtons (List.map func [(4,Club),(4,Diamond),(4,Heart),(4,Spade),(4,NoTrump)])
        level5 = attachBidBoxButtons (List.map func [(5,Club),(5,Diamond),(5,Heart),(5,Spade),(5,NoTrump)])
        level6 = attachBidBoxButtons (List.map func [(6,Club),(6,Diamond),(6,Heart),(6,Spade),(6,NoTrump)])
        level7 = attachBidBoxButtons (List.map func [(7,Club),(7,Diamond),(7,Heart),(7,Spade),(7,NoTrump)]) in
    let pass = 
            case model.bidSequence of
                [] -> attachBidBoxButtons (List.map func [(0,Pass)])
                _ -> attachBidBoxButtons (List.map func [(8,Pass)]) in
    [   Html.div [] level1,
        Html.div [] level2,
        Html.div [] level3,
        Html.div [] level4,
        Html.div [] level5,
        Html.div [] level6,
        Html.div [] level7,
        Html.div [] pass]



isBidAllowed : Bid -> Model -> Bool
isBidAllowed (newLevel, newSuit) model =
    if (((model.passes == 1) && ((List.length model.bidSequence) > 1)) || (model.passes == 2)) then False
    else
        let length = List.length model.bidSequence in
        case List.head (List.drop (length - 1) model.bidSequence) of
            Nothing -> True
            Just (oldLevel, oldSuit) ->
                if newLevel > oldLevel then True
                else if newLevel < oldLevel then False
                else (suitToInt newSuit) > (suitToInt oldSuit)

suitToInt : Suit -> Int
suitToInt suit =
    case suit of
        Club -> 0
        Diamond -> 1
        Heart -> 2
        Spade -> 3
        NoTrump -> 4
        Pass -> 5

bidString : Int -> String
bidString i =
    if i == 35 then "X"
    else if i == 36 then "XX"
    else if i == 37 then "P"
    else 
        let level = (i // 5) + 1 in
        let displayLevel = String.fromInt level in
        let suit = modBy 5 i in
        let displaySuit = case suit of
                        0 -> "C"
                        1 -> "D"
                        2 -> "H"
                        3 -> "S"
                        4 -> "NT" 
                        _ -> Debug.todo "suit faild" in
        displayLevel ++ displaySuit                

searchBoxFunction : String -> Msg
searchBoxFunction string =
  case String.right 1 string of
    "\n" -> ChangeBiddingSequence (stringToSequence (String.dropRight 1 string))
      
    _ -> NoUpdate
      
makeButtonList : BidSequence -> BidSequence -> List (Html Msg)
makeButtonList prevBids futureBids =
  case futureBids of
   [] -> [] 
   bid::rest -> Html.button [onClick (ChangeBiddingSequence (prevBids++[bid]))] [Html.text (bidToString bid)]::(makeButtonList (prevBids++[bid]) rest)

displayNextBids : BiddingRules -> BidSequence -> List (Html Msg)
displayNextBids system history =
  case (traverseSystem (Just system) history) of
    Nothing -> []
    Just nextBids -> (List.map (displayBid system history) nextBids)

newBidFunction : BiddingRules -> BidSequence -> String -> Msg
newBidFunction system history string =
  case String.right 1 string of
    "\n" -> case stringToBid (String.dropRight 1 string) of
      Nothing -> NoUpdate
      Just bid -> (UpdateSystem (defineBid system (history++[bid]) [allHands]))
      --Just bid -> (DebugString (bidToString bid))
      --Just bid -> (UpdateSystem (Just (testSystem bid)))
      
    _ -> NoUpdate
      
displayBid : BiddingRules -> BidSequence -> BidDefinition -> Html Msg
displayBid system history (BidDefinition bid) =
 let newHistory = history ++ [bid.bidValue] in
 let followingBids = Html.button [onClick (ChangeBiddingSequence (history++[bid.bidValue]))] [Html.text (bidToString bid.bidValue)] in
 let prioritize = Html.button [onClick (UpdateSystem (prioritizeBid system (history++[bid.bidValue])))] [Html.text ("Increase Priority")] in
 let deprioritize = Html.button [onClick (UpdateSystem (deprioritizeBid system (history++[bid.bidValue])))] [Html.text ("Decrease Priority")] in
 --let modify = Html.button [] [Html.text "Modify"] in
 let pointRow =  Html.div [] [Html.text "Points:",
                 Html.textarea [onInput (\string -> UpdateSystem (modifyBid system newHistory (editLowerPointTo (String.toInt string))))] [Html.text (getLowerPoint system newHistory)],
                 Html.text "to",
                 Html.textarea [onInput (\string -> UpdateSystem (modifyBid system history (editUpperPointTo (String.toInt string))))] [Html.text (getUpperPoint system newHistory)]] in
 let spadeRow = Html.div [] [Html.text "Spades:"] in
 let heartRow = Html.div [] [Html.text "Hearts:"] in
 let diamondRow = Html.div [] [Html.text "Diamonds:"] in
 let clubRow = Html.div [] [Html.text "Clubs:"] in
 let meaningTable = Html.div [] [pointRow, spadeRow, heartRow, diamondRow, clubRow]  in
 let delete = Html.button [onClick (UpdateSystem (removeBid system (history++[bid.bidValue])))] [Html.text "Delete Bid"] in
 Html.div [] [followingBids, meaningTable, prioritize, deprioritize, delete]
 --Html.div [] [followingBids, modify]

testSystem : Bid -> BiddingRules
testSystem bid = [BidDefinition {requirements = [], bidValue = (1, Spade), subsequentBids = []}]

editLowerPointTo : Maybe Int -> List HandRange -> Maybe (List HandRange)
editLowerPointTo maybeLowerBound handRangeList =
    case (maybeLowerBound, handRangeList) of
        (Just lowerBound, handRange::rest) -> let (prevPointMin, prevPointMax) = handRange.points in Just ({handRange | points = (lowerBound, prevPointMax)}::rest)
        _ -> Nothing

editUpperPointTo : Maybe Int -> List HandRange -> Maybe (List HandRange)
editUpperPointTo maybeUpperBound handRangeList =
    case (maybeUpperBound, handRangeList) of
        (Just upperBound, handRange::rest) -> let (prevPointMin, prevPointMax) = handRange.points in Just ({handRange | points = (prevPointMin, upperBound)}::rest)
        _ -> Nothing           

getLowerPoint : BiddingRules -> List Bid -> String
getLowerPoint system history =
    case getBid system history of
        Nothing -> ""
        Just (BidDefinition bid) -> case bid.requirements of
            [] -> ""
            meaning::rest -> let (lowerPoint, upperPoint) = meaning.points in String.fromInt lowerPoint

getUpperPoint : BiddingRules -> List Bid -> String
getUpperPoint system history =
    case getBid system history of
        Nothing -> ""
        Just (BidDefinition bid) -> case bid.requirements of
            [] -> ""
            meaning::rest -> let (lowerPoint, upperPoint) = meaning.points in String.fromInt upperPoint
            

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch []


-- MAIN

type alias Flags = ()

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)

main : Program () Model Msg
main =
  Browser.element {init = init,
                   view = view,
                   update = update,
                   subscriptions = subscriptions}