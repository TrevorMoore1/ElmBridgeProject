module GUI exposing (main)

import Bidding exposing (..)
import Deal exposing (Card, shuffle, separate, handRecords, newDeal)
import Browser
import Bidding exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onInput, onMouseOver, onMouseLeave)
import Task exposing (..)
import Time exposing (..)


-- MODEL

type Location = Practice | Edit

type alias Model = {nextSeed : Int,
                    passes : Int,
                    location : Location,
                    system : BiddingRules,
                    bidSequence : BidSequence,
                    bidExplanation : String,
                    debug : String}

initModel = {   nextSeed = 0, 
                passes = 0,
                location = Practice,
                system = [],
                bidSequence = [],
                bidExplanation = "",
                debug = "Debug"}


-- UPDATE

type Msg =
    RequestTime
  | ReceiveTime Posix
  | BidsMade BidSequence
  | DisplayExplanation String
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
    DisplayExplanation expl -> ({model | bidExplanation = expl}, Cmd.none)
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
            let bidsMadeButtons = Html.div [] (makeBidsMadeButtons model model.bidSequence) in
            case handString of
                n :: e :: s :: w :: [] ->
                    Html.div [] ([  Html.div [] [practice, edit],
                                    redeal,
                                    Html.div [monoStyle] [Html.text n],
                                    Html.div [monoStyle] [Html.text e],
                                    Html.div [monoStyle] [Html.text s],
                                    Html.div [monoStyle] [Html.text w]]
                                    ++ availableBids
                                    ++ [bidsMadeButtons]
                                    ++ [Html.p [] [Html.text model.bidExplanation]])
                _ -> Debug.todo "view failed"
        Edit ->
            let practice = Html.button [onClick (Goto Practice)] [Html.text "Practice"] in
            let edit = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Edit System"] in
            let buttonList = Html.button [onClick (Goto Edit)] [Html.text "Begin"]::(makeButtonList [] model.bidSequence) in
            let searchBox = Html.textarea [onInput searchBoxFunction] [] in
            let instructions = Html.p [] [Html.text "Create new bid"] in
            --let debugMessage = Html.p [] [Html.text model.debug] in
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

-- PRACTICE MODE FUNCTIONS -----------------------------------------------------------

makeBidsMadeButtons : Model -> BidSequence -> List (Html Msg)
makeBidsMadeButtons model bidSequence =
    case bidSequence of
        [] -> []
        bid :: rest ->
            let color = 
                    case Tuple.second bid of
                        Heart -> "red"
                        Diamond -> "red"
                        _ -> "black" in
            let stringBid = bidToString bid in
            (Html.button [  onMouseOver (DisplayExplanation (explanationToString bid model.bidSequence model.system)),
                            onMouseLeave (DisplayExplanation "")] 
                            [Html.div [] [  Html.span [] [Html.text (String.left 1 stringBid)],
                                            Html.span [Html.Attributes.style "color" color] [Html.text (String.dropLeft 1 stringBid)]]]) 
            :: (makeBidsMadeButtons model rest)


makeBidBoxButton : Bid -> Html Msg
makeBidBoxButton bid =
    let color = 
            case Tuple.second bid of
                Heart -> "red"
                Diamond -> "red"
                _ -> "black" in
    let stringBid = bidToString bid in
    Html.button [onClick (BidsMade [bid])] 
                [Html.div [] [  Html.span [] [Html.text (String.left 1 stringBid)],
                                Html.span [Html.Attributes.style "color" color] [Html.text (String.dropLeft 1 stringBid)]]]

makeBidBoxButtonInvis : Bid -> Html Msg
makeBidBoxButtonInvis bid =
    Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text (bidToString bid)]

attachBidBoxButtons : List (Bid, Bool) -> List (Html Msg)
attachBidBoxButtons bids =
    case bids of
        [] -> []
        (bid, allowed) :: rest ->
            if allowed 
                then (makeBidBoxButton bid) :: attachBidBoxButtons rest
            else (makeBidBoxButtonInvis bid) :: attachBidBoxButtons rest 

bidDisplay : Model -> List (Html Msg)
bidDisplay model =
    let makeTupleFunc = (\bid -> (bid, isBidAllowed bid model)) in
    let attachFunc = (\i -> attachBidBoxButtons (List.map makeTupleFunc [(i,Club),(i,Diamond),(i,Heart),(i,Spade),(i,NoTrump)])) in
    let divFunc = (\lev -> Html.div [] lev) in
    let pass = 
            case model.bidSequence of
                [] -> attachBidBoxButtons (List.map makeTupleFunc [(0,Pass)])
                _ -> attachBidBoxButtons (List.map makeTupleFunc [(8,Pass)]) in
    ((List.range 1 7)
        |> List.map attachFunc
        |> List.map divFunc)
    ++ [Html.div [] pass]

explanationToString : Bid -> BidSequence -> List BidDefinition -> String
explanationToString currentBid bidsMade system =
    case bidsMade of
        [] -> ""
        bid :: restBids ->
            case system of
                [] -> "Undefined"
                BidDefinition def :: restDef -> 
                    if def.bidValue == bid then
                        if bid == currentBid then handRangeToString def.requirements
                        else explanationToString currentBid restBids def.subsequentBids
                    else explanationToString currentBid bidsMade restDef 

handRangeToString : List HandRange -> String
handRangeToString ranges =
    case ranges of
        [] -> ""
        range :: restRanges ->
            let firstString = 
                        String.fromInt (Tuple.first range.points) ++ "-" ++ String.fromInt (Tuple.second range.points) ++ "points, "
                    ++  String.fromInt (Tuple.first range.spades) ++ "-" ++ String.fromInt (Tuple.second range.spades) ++ "spades, "
                    ++  String.fromInt (Tuple.first range.hearts) ++ "-" ++ String.fromInt (Tuple.second range.hearts) ++ "hearts, "
                    ++  String.fromInt (Tuple.first range.diamonds) ++ "-" ++ String.fromInt (Tuple.second range.diamonds) ++ "diamonds, "
                    ++  String.fromInt (Tuple.first range.clubs) ++ "-" ++ String.fromInt (Tuple.second range.clubs) ++ "clubs" in
            if (List.isEmpty restRanges)
                then firstString
                else firstString ++ " OR " ++ handRangeToString restRanges

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

-- EDIT MODE FUNCTIONS ---------------------------------------------------------------          

searchBoxFunction : String -> Msg
searchBoxFunction string =
  case String.right 1 string of
    "\n" -> BidsMade (stringToSequence (String.dropRight 1 string))
      
    _ -> NoUpdate
      
makeButtonList : BidSequence -> BidSequence -> List (Html Msg)
makeButtonList prevBids futureBids =
  case futureBids of
   [] -> [] 
   bid::rest -> Html.button [onClick (BidsMade (prevBids++[bid]))] [Html.text (bidToString bid)]::(makeButtonList (prevBids++[bid]) rest)

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
      Just bid -> (UpdateSystem (defineBid system (history++[bid]) []))
      --Just bid -> (DebugString (bidToString bid))
      --Just bid -> (UpdateSystem (Just (testSystem bid)))
      
    _ -> NoUpdate
      
displayBid : BiddingRules -> BidSequence -> BidDefinition -> Html Msg
displayBid system history (BidDefinition bid) =
 let followingBids = Html.button [onClick (BidsMade (history++[bid.bidValue]))] [Html.text (bidToString bid.bidValue)] in
 let prioritize = Html.button [onClick (UpdateSystem (prioritizeBid system history))] [Html.text ("Increase Priority")] in
 let deprioritize = Html.button [onClick (UpdateSystem (deprioritizeBid system history))] [Html.text ("Decrease Priority")] in
 let modify = Html.button [] [Html.text "Modify"] in
 let delete = Html.button [onClick (UpdateSystem (removeBid system history))] [Html.text "Delete Bid"] in
 Html.div [] [followingBids, modify, prioritize, deprioritize, delete]
 --Html.div [] [followingBids, modify]

testSystem : Bid -> BiddingRules
testSystem bid = [BidDefinition {requirements = [], bidValue = (1, Spade), subsequentBids = []}]

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