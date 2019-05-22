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

type Location = BidPractice | Loc BidSequence

type alias Model = {nextSeed : Int,
                    currentBid : Int,
                    xAllowed : Bool,
                    xxAllowed : Bool,
                    passes : Int,
                    location : Location,
                    system : BiddingRules}
-- Suited bids are #0-34, then X, XX, P

initModel = {   nextSeed = 0, 
                currentBid = -1, 
                xAllowed = False, 
                xxAllowed = False,
                passes = 0,
                location = BidPractice,
                system = []}


-- UPDATE

type Msg =
    RequestTime
  | ReceiveTime Posix
  | NewBid Int
  | Goto Location
  | UpdateSystem (Maybe BiddingRules)
  | NoUpdate

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestTime -> (model, Task.perform ReceiveTime Time.now)
    ReceiveTime time ->
        let {nextSeed} = model in
        let newSeed = Time.posixToMillis time in
        ({model | nextSeed = newSeed, currentBid = -1, xAllowed = False, xxAllowed = False, passes = 0}, Cmd.none)
    NewBid bid ->
        let {nextSeed, currentBid, xAllowed, xxAllowed, passes} = model in
        case bid of
            35 -> ({model | xAllowed = False, xxAllowed = True, passes = 0}, Cmd.none)
            36 -> ({model | xAllowed = False, xxAllowed = False, passes = 0}, Cmd.none)
            37 -> ({model | passes = passes + 1}, Cmd.none)
            _ -> ({model | currentBid = bid, xAllowed = True, xxAllowed = False, passes = 0}, Cmd.none)
    Goto newLocation -> ({model | location = newLocation}, Cmd.none)
    UpdateSystem maybeSystem ->
        case maybeSystem of
            Nothing -> (model, Cmd.none)
            Just newSystem ->
                ({model | system = newSystem}, Cmd.none)
    NoUpdate -> (model, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
    case model.location of
        BidPractice -> 
            let redeal = Html.button [onClick RequestTime] [Html.text "Redeal"] in
            let bidPractice = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Practice"] in
            let edit = Html.button [onClick (Goto (Loc []))] [Html.text "Edit System"] in
            let availableBids = bidDisplay model in
            let handString = Deal.newDeal model.nextSeed in
            let monoStyle = Html.Attributes.style "font-family" "courier" in
            case handString of
                n :: e :: s :: w :: [] ->
                    Html.div [] ([  Html.div [] [bidPractice, edit],
                                    redeal, 
                                    Html.div [monoStyle] [Html.text n],
                                    Html.div [monoStyle] [Html.text e],
                                    Html.div [monoStyle] [Html.text s],
                                    Html.div [monoStyle] [Html.text w]]
                                    ++ availableBids)
                _ -> Debug.todo "view failed"
        Loc bidSequence ->
            let bidPractice = Html.button [] [Html.text "Practice"] in
            let edit = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Edit System"] in
            let buttonList = Html.button [onClick (Goto (Loc []))] [Html.text "Begin"]::(makeButtonList [] bidSequence) in
            let searchBox = Html.textarea [onInput searchBoxFunction] [] in
            let nextBids = displayNextBids model.system bidSequence in
            Html.div [] [Html.p [] buttonList, Html.p [] [searchBox], Html.p [] nextBids]


makeBidButton : Int -> Html Msg
makeBidButton bid =
    Html.button [onClick (NewBid bid)] [Html.text (bidString bid)]

makeBidButtonInvis : Int -> Html Msg
makeBidButtonInvis bid =
    Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text (bidString bid)]

attachBidButtons : List (Int, Bool) -> List (Html Msg)
attachBidButtons bids =
    case bids of
        [] -> []
        (bid, allowed) :: rest ->
            if allowed then (makeBidButton bid) :: attachBidButtons rest
            else (makeBidButtonInvis bid) :: attachBidButtons rest 

bidDisplay : Model -> List (Html Msg)
bidDisplay model =
    let func = (\i -> (i, isBidAllowed i model)) in
    let level1 = attachBidButtons (List.map func (List.range 0 4))
        level2 = attachBidButtons (List.map func (List.range 5 9))
        level3 = attachBidButtons (List.map func (List.range 10 14))
        level4 = attachBidButtons (List.map func (List.range 15 19))
        level5 = attachBidButtons (List.map func (List.range 20 24))
        level6 = attachBidButtons (List.map func (List.range 25 29))
        level7 = attachBidButtons (List.map func (List.range 30 34))
        level8 = attachBidButtons (List.map func (List.range 35 37)) in
    [   Html.div [] level1,
        Html.div [] level2,
        Html.div [] level3,
        Html.div [] level4,
        Html.div [] level5,
        Html.div [] level6,
        Html.div [] level7,
        Html.div [] level8]



isBidAllowed : Int -> Model -> Bool
isBidAllowed bid model =
    let {currentBid, xAllowed, xxAllowed, passes} = model in
    if (((currentBid > -1) && (passes == 3)) || (passes == 4)) then False
    else if bid <= currentBid then False
    else if ((bid == 35) && (not xAllowed)) then False
    else if ((bid == 36) && (not xxAllowed)) then False
    else True

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