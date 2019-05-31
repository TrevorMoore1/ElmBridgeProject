module GUI exposing (main)

import Bidding exposing (..)
import Deal exposing (Card, shuffle, separate, handRecords, newDeal)
import Browser
import Bidding exposing (..)
import Html exposing (Html)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick, onInput, onMouseOver, onMouseLeave)
import Task exposing (..)
import Time exposing (..)


-- MODEL

type Location = Practice | Edit

type alias Model = {myHand : String,
                    yourHand : String, 
                    passes : Int,
                    location : Location,
                    system : BiddingRules,
                    subSystem : BiddingRules,
                    bidSequence : BidSequence,
                    bidExplanation : String,
                    debug : String}

initModel = {   myHand = "",
                yourHand = "",
                passes = 0,
                location = Practice,
                system = [],
                subSystem = [],
                bidSequence = [],
                bidExplanation = "",
                debug = "Debug"}


-- UPDATE

type Msg =
    RequestTime
  | ReceiveTime Posix
  | BidsMade BidSequence
  | DisplayExplanation String
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
        let newSeed = Time.posixToMillis time in
        let handString = Deal.newDeal newSeed in
        let myHandString = 
                case List.head handString of
                    Nothing -> Debug.todo "myHandString error"
                    Just myHand -> myHand in
        let yourHandString = 
                case List.head (List.drop 1 handString) of
                    Nothing -> Debug.todo "yourHandString error"
                    Just yourHand -> yourHand in
        ({model | myHand = myHandString, yourHand = yourHandString, passes = 0, bidSequence = [], subSystem = model.system}, Cmd.none)
    BidsMade bids ->
        let tempModel = 
                case bids of
                    (level, Pass)::rest -> {model | passes = model.passes + 1, bidSequence = model.bidSequence ++ bids}
                    (level, _)::rest -> {model | passes = 0, bidSequence = model.bidSequence ++ bids}
                    _ -> model in
        case makeBid tempModel.subSystem (stringToType tempModel.yourHand {spades = 0, hearts = 0, diamonds = 0, clubs = 0, points = 0} Pass) of
                Nothing -> ({tempModel | bidExplanation = "Sorry, I don't know what to bid!"}, Cmd.none)
                Just (bid, subsequentBids) -> ({tempModel | subSystem = subsequentBids, bidSequence = tempModel.bidSequence ++ [bid]}, Cmd.none)
    DisplayExplanation expl -> ({model | bidExplanation = expl}, Cmd.none)
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
            
            let monoStyle = Html.Attributes.style "font-family" "courier" in
            let bidsMadeButtons = Html.div [] (makeBidsMadeButtons model model.bidSequence) in
            Html.div [] ([  Html.div [] [practice, edit],
                                    redeal,
                                    Html.div [] [Html.br [] []],
                                    Html.div [monoStyle] [Html.text model.myHand],
                                    Html.div [monoStyle] [Html.text model.yourHand],
                                    Html.div [] [Html.br [] []]]
                                    ++ availableBids
                                    ++ [Html.br [] []]
                                    ++ [bidsMadeButtons]
                                    ++ [Html.p [] [Html.text model.bidExplanation]])
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


makeBidBoxButton : Bid -> Model -> Html Msg
makeBidBoxButton bid model =
    let color = 
            case Tuple.second bid of
                Heart -> "red"
                Diamond -> "red"
                _ -> "black" in
    let stringBid = bidToString bid in
    Html.button [   onClick (BidsMade [bid]),
                    onMouseOver (DisplayExplanation (explanationToString bid (model.bidSequence ++ [bid]) model.system)),
                    onMouseLeave (DisplayExplanation "")] 
                [Html.div [] [Html.span [] [Html.text (String.left 1 stringBid)],
                              Html.span [Html.Attributes.style "color" color] [Html.text (String.dropLeft 1 stringBid)]]]

makeBidBoxButtonInvis : Bid -> Html Msg
makeBidBoxButtonInvis bid =
    Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text (bidToString bid)]

attachBidBoxButtons : List (Bid, Bool) -> Model -> List (Html Msg)
attachBidBoxButtons bids model =
    case bids of
        [] -> []
        (bid, allowed) :: rest ->
            if allowed 
                then (makeBidBoxButton bid model) :: attachBidBoxButtons rest model
            else (makeBidBoxButtonInvis bid) :: attachBidBoxButtons rest model

bidDisplay : Model -> List (Html Msg)
bidDisplay model =
    let makeTupleFunc = (\bid -> (bid, isBidAllowed bid model)) in
    let attachFunc = (\i -> attachBidBoxButtons (List.map makeTupleFunc [(i,Club),(i,Diamond),(i,Heart),(i,Spade),(i,NoTrump)]) model) in
    let divFunc = (\lev -> Html.div [] lev) in
    let pass = 
            case model.bidSequence of
                [] -> attachBidBoxButtons (List.map makeTupleFunc [(0,Pass)]) model
                _ -> attachBidBoxButtons (List.map makeTupleFunc [(8,Pass)]) model in
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
                        String.fromInt (Tuple.first range.points) ++ "-" ++ String.fromInt (Tuple.second range.points) ++ " points, "
                    ++  String.fromInt (Tuple.first range.spades) ++ "-" ++ String.fromInt (Tuple.second range.spades) ++ " spades, "
                    ++  String.fromInt (Tuple.first range.hearts) ++ "-" ++ String.fromInt (Tuple.second range.hearts) ++ " hearts, "
                    ++  String.fromInt (Tuple.first range.diamonds) ++ "-" ++ String.fromInt (Tuple.second range.diamonds) ++ " diamonds, "
                    ++  String.fromInt (Tuple.first range.clubs) ++ "-" ++ String.fromInt (Tuple.second range.clubs) ++ " clubs" in
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
 let meaningTable = Html.div [] (List.map (makeSuitRow system newHistory) [NoTrump, Spade, Heart, Diamond, Club])  in
 let delete = Html.button [onClick (UpdateSystem (removeBid system (history++[bid.bidValue])))] [Html.text "Delete Bid"] in
 Html.div [] [followingBids, meaningTable, prioritize, deprioritize, delete]
 --Html.div [] [followingBids, modify]

makeSuitRow : BiddingRules -> BidSequence -> Suit -> Html Msg
makeSuitRow system newHistory suit = 
    let label = 
            case suit of
                NoTrump -> "Points:"
                Spade -> "Spades:"    
                Heart -> "Hearts:"
                Diamond -> "Diamonds:"
                Club -> "Clubs:"
                Pass -> ""
    in
    Html.div [] [Html.text label,
    Html.input [onInput (\string -> UpdateSystem (modifyBid system newHistory (editLowerTo suit (toIntEmpty string)))), Html.Attributes.value (getLower suit system newHistory)] [],
    Html.text "to",
    Html.input [onInput (\string -> UpdateSystem (modifyBid system newHistory (editUpperTo suit (toIntEmpty string)))), Html.Attributes.value (getUpper suit system newHistory)] []]

editLowerTo : Suit -> Maybe Int -> List HandRange -> Maybe (List HandRange)
editLowerTo suit maybeLowerBound handRangeList = 
    case (maybeLowerBound, handRangeList) of
        (Just lowerBound, handRange::rest) -> case suit of
            NoTrump -> let (prevMin, prevMax) = handRange.points in Just ({handRange | points = (lowerBound, prevMax)}::rest)
            Spade -> let (prevMin, prevMax) = handRange.spades in Just ({handRange | spades = (lowerBound, prevMax)}::rest)
            Heart -> let (prevMin, prevMax) = handRange.hearts in Just ({handRange | hearts = (lowerBound, prevMax)}::rest)
            Diamond -> let (prevMin, prevMax) = handRange.diamonds in Just ({handRange | diamonds = (lowerBound, prevMax)}::rest)
            Club -> let (prevMin, prevMax) = handRange.clubs in Just ({handRange | clubs = (lowerBound, prevMax)}::rest)  
            _ -> Just handRangeList
        _ -> Nothing

editUpperTo : Suit -> Maybe Int -> List HandRange -> Maybe (List HandRange)
editUpperTo suit maybeUpperBound handRangeList = 
    case (maybeUpperBound, handRangeList) of
        (Just upperBound, handRange::rest) -> case suit of
            NoTrump -> let (prevMin, prevMax) = handRange.points in Just ({handRange | points = (prevMin, upperBound)}::rest)
            Spade -> let (prevMin, prevMax) = handRange.spades in Just ({handRange | spades = (prevMin, upperBound)}::rest)
            Heart -> let (prevMin, prevMax) = handRange.hearts in Just ({handRange | hearts = (prevMin, upperBound)}::rest)
            Diamond -> let (prevMin, prevMax) = handRange.diamonds in Just ({handRange | diamonds = (prevMin, upperBound)}::rest)
            Club -> let (prevMin, prevMax) = handRange.clubs in Just ({handRange | clubs = (prevMin, upperBound)}::rest)  
            _ -> Just handRangeList
        _ -> Nothing        

getLower : Suit -> BiddingRules -> List Bid -> String
getLower suit system history =
    case getBid system history of
        Nothing -> "a"
        Just (BidDefinition bid) -> case bid.requirements of
            [] -> "b"
            meaning::rest -> case suit of
                NoTrump -> let (lower, upper) = meaning.points in String.fromInt lower
                Spade -> let (lower, upper) = meaning.spades in String.fromInt lower
                Heart -> let (lower, upper) = meaning.hearts in String.fromInt lower
                Diamond -> let (lower, upper) = meaning.diamonds in String.fromInt lower
                Club -> let (lower, upper) = meaning.clubs in String.fromInt lower 
                _ -> "c"
                     

getUpper : Suit -> BiddingRules -> List Bid -> String
getUpper suit system history =
    case getBid system history of
        Nothing -> "a"
        Just (BidDefinition bid) -> case bid.requirements of
            [] -> "b"
            meaning::rest -> case suit of
                NoTrump -> let (lower, upper) = meaning.points in String.fromInt upper
                Spade -> let (lower, upper) = meaning.spades in String.fromInt upper
                Heart -> let (lower, upper) = meaning.hearts in String.fromInt upper
                Diamond -> let (lower, upper) = meaning.diamonds in String.fromInt upper
                Club -> let (lower, upper) = meaning.clubs in String.fromInt upper
                _ -> "c"
            
toIntEmpty : String -> Maybe Int
toIntEmpty string =
    case string of
        "" -> Just 0
        _ -> String.toInt string 


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