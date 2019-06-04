port module GUI exposing (..)

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

type Location = Practice | Edit | Select

type alias Model = {myHand : String,
                    yourHand : String, 
                    passes : Int,
                    location : Location,
                    system : BiddingRules,
                    subSystem : BiddingRules,
                    bidSequence : BidSequence,
                    bidExplanation : String,
                    debug : String,
                    systemList : List (String, BiddingRules),
                    systemIndex : Int}

initModel : Flags -> Model
initModel flag =
                 {   myHand = "",
                yourHand = "",
                passes = 0,
                location = Select,
                system = [],
                systemIndex = 0,
                systemList = case flag of
                    Nothing -> []
                    Just flagString -> parseWithNames flagString ""
                ,
                subSystem = [],
                bidSequence = [],
                bidExplanation = "",
                debug = case flag of
                    Nothing -> "NoFlag"
                        
                    Just flagstring -> String.fromInt (List.length (stringToSystemList flagstring))
                }



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
  | PracticeIn Int
  | EditIn Int
  | CreateSystem
  | DeleteSystem Int
  | UpdateName Int String

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
                let (systemName, oldSystem) = case List.drop model.systemIndex model.systemList of
                                                    [] -> ("", [])
                                                    elem::rest -> elem
                in 
                let newSystemList = (List.take model.systemIndex model.systemList) ++ [(systemName, newSystem)] ++ (List.drop ((model.systemIndex)+1) model.systemList) in 
                ({model | system = newSystem, systemList = newSystemList, debug = (namedSystemListToString newSystemList) ++ (String.fromInt model.systemIndex) }, saveSystems (namedSystemListToString newSystemList))
                --({model | debug = "Updat}, Cmd.none)
    NoUpdate -> (model, Cmd.none)
    DebugString string -> ({model | debug = string}, Cmd.none)
    PracticeIn index -> ({model | passes = 0, location = Practice, bidSequence = [], systemIndex = index,
                        system = case List.head (List.drop index model.systemList) of
                            Nothing -> []    
                            Just (_, bidSystem) -> bidSystem
                        }, Cmd.none)
    EditIn index -> ({model | passes = 0, location = Edit, bidSequence = [], systemIndex = index,
                        system = case List.head (List.drop index model.systemList) of
                            Nothing -> []    
                            Just (_, bidSystem) -> bidSystem
                        }, Cmd.none)
    CreateSystem -> let newSystemList = model.systemList ++ [("", [])] in ({model | systemList = newSystemList}, saveSystems (namedSystemListToString newSystemList))
    DeleteSystem index -> let newSystemList = List.take index model.systemList ++ List.drop (index+1) model.systemList in ({model | systemList = newSystemList}, saveSystems (namedSystemListToString newSystemList))
    UpdateName index name -> let (systemName, oldSystem) = case List.take index model.systemList of
                                                    [] -> ("", [])
                                                    elem::rest -> elem
                             in 
                             let newSystemList = (List.take index model.systemList) ++ [(name, oldSystem)] ++ (List.drop (index+1) model.systemList) in
                             ({model | systemList = newSystemList, debug = (name ++ " | " ++ String.fromInt index ++ " | " ++ String.concat (List.map (\(x,y) -> x) newSystemList))}, saveSystems (namedSystemListToString newSystemList))
        



-- VIEW

view : Model -> Html Msg
view model =
    case model.location of
        Practice -> 
            let redeal = Html.button [onClick RequestTime] [Html.text "Redeal"] in
            let select = Html.button [onClick (Goto Select)] [Html.text "Select System"] in
            let practice = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Practice"] in
            let edit = Html.button [onClick (Goto Edit)] [Html.text "Edit System"] in
            let availableBids = bidDisplay model in
            
            let monoStyle = Html.Attributes.style "font-family" "courier" in
            let bidsMadeButtons = Html.div [] (makeBidsMadeButtons model model.bidSequence) in
            Html.div [] ([  Html.div [] [select, practice, edit],
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
            let select = Html.button [onClick (Goto Select)] [Html.text "Select System"] in
            let practice = Html.button [onClick (Goto Practice)] [Html.text "Practice"] in
            let edit = Html.button [Html.Attributes.style "visibility" "hidden"] [Html.text "Edit System"] in
            let buttonList = Html.button [onClick (ChangeBiddingSequence [])] [Html.text "Begin"]::(makeButtonList [] model.bidSequence) in
            let searchBox = Html.textarea [onInput searchBoxFunction] [] in
            let instructions = Html.p [] [Html.text "Create new bid"] in
            let debugMessage = Html.p [] [Html.text model.debug] in
            --let debugMessage = Html.textarea [] [Html.text (getLowerPoint model.system [(1,Spade)])] in
            let newBidBox = [Html.textarea [onInput (newBidFunction model.system model.bidSequence)] []] in
            --let newBidBox = [Html.textarea [onInput DebugString] []] in
            let nextBids = displayNextBids model.system model.bidSequence in
            Html.div [] [   Html.div [] [select, practice, edit],
                            Html.div [] buttonList, 
                            Html.div [] [searchBox], 
                            instructions,
                            Html.div [] newBidBox,
                            Html.div [] nextBids
                            ]
        Select -> 
            Html.div [] ((createSystemSelect model.systemList 0) ++ [Html.button [onClick CreateSystem] [Html.text "Create New System"]])


-- SELECT MODE FUNCTIONS ------------------------------------------------------------

createSystemSelect : List (String, BiddingRules) -> Int -> List (Html Msg)
createSystemSelect systemList index = case systemList of
    [] -> []    
    system::rest -> let (systemName, oldSystem) = case systemList of
                                                                        [] -> ("", [])
                                                                        elem::_ -> elem
                                     in 
                     [Html.div [] ([Html.input [onInput (UpdateName index), Html.Attributes.value systemName] []] ++
                                   [Html.text (" : ")] ++
                                   [Html.button [onClick (PracticeIn index)] [Html.text "Practice"]] ++ 
                                   [Html.button [onClick (EditIn index)] [Html.text "Edit"]] ++
                                   [Html.button [onClick (DeleteSystem index)] [Html.text "Delete"]])] ++
                                  createSystemSelect rest (index+1)



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
 let meaningTable = Html.div [] (List.map (\suit -> Html.div [] (makeSuitRow system newHistory suit)) [NoTrump, Spade, Heart, Diamond, Club])  in
 let delete = Html.button [onClick (UpdateSystem (removeBid system (history++[bid.bidValue])))] [Html.text "Delete Bid"] in
 let addMeaning = Html.button [onClick (UpdateSystem (modifyBid system newHistory (\prevDefs -> Just (prevDefs ++ [allHands]))))] [Html.text "Add New Meaning"] in
 let deleteList = makeDeleteList system newHistory bid.requirements 0 in
 Html.div [] ([followingBids, meaningTable, prioritize, deprioritize, addMeaning] ++ deleteList ++ [delete])
 --Html.div [] [followingBids, modify]

makeDeleteList : BiddingRules -> BidSequence -> List HandRange -> Int -> List (Html Msg)
makeDeleteList system newHistory requirements index =
    case requirements of
        [] -> []
        _::rest -> [Html.button [onClick (UpdateSystem (modifyBid system newHistory (\prevDefs -> Just((List.take index prevDefs) ++ (List.drop (index+1) prevDefs)))))]
                                [Html.text ("Delete Meaning " ++ String.fromInt (index + 1))]] ++ makeDeleteList system newHistory rest (index+1)


makeSuitRow : BiddingRules -> BidSequence -> Suit -> List (Html Msg)
makeSuitRow system newHistory suit = 
    case getBid system newHistory of
        Nothing -> []
        Just (BidDefinition bidDef) -> makeSuitRowReqs system newHistory 0 suit bidDef.requirements

makeSuitRowReqs : BiddingRules -> BidSequence -> Int -> Suit -> (List HandRange) -> List (Html Msg)
makeSuitRowReqs system newHistory meaningNum suit reqs = 
    let label = 
            case suit of
                NoTrump -> "Points:"
                Spade -> "Spades:"    
                Heart -> "Hearts:"
                Diamond -> "Diamonds:"
                Club -> "Clubs:"
                Pass -> ""
    in case reqs of
            [] -> []
            meaning::rest -> [Html.text label,
                             Html.input [onInput (\string -> UpdateSystem (modifyBid system newHistory (editLowerTo suit (toIntEmpty string) meaningNum))), Html.Attributes.value (getLower suit system newHistory meaningNum)] [],
                             Html.text "to",
                             Html.input [onInput (\string -> UpdateSystem (modifyBid system newHistory (editUpperTo suit (toIntEmpty string) meaningNum))), Html.Attributes.value (getUpper suit system newHistory meaningNum)] []]
                             ++ makeSuitRowReqs system newHistory (meaningNum+1) suit rest

   
editLowerTo : Suit -> Maybe Int -> Int -> List HandRange -> Maybe (List HandRange)
editLowerTo suit maybeLowerBound meaningNum handRangeList = 
    let listPrefix = List.take meaningNum handRangeList in
    case (maybeLowerBound, List.drop meaningNum handRangeList) of
        (Just lowerBound, handRange::rest) -> case suit of
            NoTrump -> let (prevMin, prevMax) = handRange.points in Just (listPrefix++({handRange | points = (lowerBound, prevMax)}::rest))
            Spade -> let (prevMin, prevMax) = handRange.spades in Just (listPrefix++({handRange | spades = (lowerBound, prevMax)}::rest))
            Heart -> let (prevMin, prevMax) = handRange.hearts in Just (listPrefix++({handRange | hearts = (lowerBound, prevMax)}::rest))
            Diamond -> let (prevMin, prevMax) = handRange.diamonds in Just (listPrefix++({handRange | diamonds = (lowerBound, prevMax)}::rest))
            Club -> let (prevMin, prevMax) = handRange.clubs in Just (listPrefix++({handRange | clubs = (lowerBound, prevMax)}::rest))
            _ -> Just handRangeList
        _ -> Nothing

editUpperTo : Suit -> Maybe Int -> Int -> List HandRange -> Maybe (List HandRange)
editUpperTo suit maybeUpperBound meaningNum handRangeList = 
    let listPrefix = List.take meaningNum handRangeList in
    case (maybeUpperBound, List.drop meaningNum handRangeList) of
        (Just upperBound, handRange::rest) -> case suit of
            NoTrump -> let (prevMin, prevMax) = handRange.points in Just (listPrefix++({handRange | points = (prevMin, upperBound)}::rest))
            Spade -> let (prevMin, prevMax) = handRange.spades in Just (listPrefix++({handRange | spades = (prevMin, upperBound)}::rest))
            Heart -> let (prevMin, prevMax) = handRange.hearts in Just (listPrefix++({handRange | hearts = (prevMin, upperBound)}::rest))
            Diamond -> let (prevMin, prevMax) = handRange.diamonds in Just (listPrefix++({handRange | diamonds = (prevMin, upperBound)}::rest))
            Club -> let (prevMin, prevMax) = handRange.clubs in Just (listPrefix++({handRange | clubs = (prevMin, upperBound)}::rest))
            _ -> Just handRangeList
        _ -> Nothing        

getLower : Suit -> BiddingRules -> List Bid -> Int -> String
getLower suit system history meaningNum =
    case getBid system history of
        Nothing -> "Error 1"
        Just (BidDefinition bid) -> case List.drop meaningNum bid.requirements of
            [] -> "Error 2"
            meaning::rest -> case suit of
                NoTrump -> let (lower, upper) = meaning.points in String.fromInt lower
                Spade -> let (lower, upper) = meaning.spades in String.fromInt lower
                Heart -> let (lower, upper) = meaning.hearts in String.fromInt lower
                Diamond -> let (lower, upper) = meaning.diamonds in String.fromInt lower
                Club -> let (lower, upper) = meaning.clubs in String.fromInt lower 
                _ -> "Error 3"
                     

getUpper : Suit -> BiddingRules -> List Bid -> Int -> String
getUpper suit system history meaningNum =
    case getBid system history of
        Nothing -> "Error 1"
        Just (BidDefinition bid) -> case List.drop meaningNum bid.requirements of
            [] -> "Error 2"
            meaning::rest -> case suit of
                NoTrump -> let (lower, upper) = meaning.points in String.fromInt upper
                Spade -> let (lower, upper) = meaning.spades in String.fromInt upper
                Heart -> let (lower, upper) = meaning.hearts in String.fromInt upper
                Diamond -> let (lower, upper) = meaning.diamonds in String.fromInt upper
                Club -> let (lower, upper) = meaning.clubs in String.fromInt upper
                _ -> "Error 3"
            
toIntEmpty : String -> Maybe Int
toIntEmpty string =
    case string of
        "" -> Just 0
        _ -> String.toInt string 

systemListToString : List BiddingRules -> String
systemListToString systems = 
    case systems of
        [] -> ""
        system::[] -> systemToString system   
        system::rest -> (systemToString system) ++ "Y" ++ (systemListToString rest)

namedSystemListToString : List (String, BiddingRules) -> String
namedSystemListToString systems = 
    case systems of
        [] -> ""
        (name,system)::[] -> (String.fromInt (String.length name)) ++ "L" ++ name ++ systemToString system
        (name,system)::rest -> (String.fromInt (String.length name)) ++ "L" ++ name ++ systemToString system ++ "Y" ++ namedSystemListToString rest
            

systemToString : BiddingRules -> String
systemToString system =
    case system of
        [] -> ""
        BidDefinition bidDef :: rest -> bidToBasicString bidDef.bidValue ++ ":" ++ requirementsToString (bidDef.requirements) ++
            "(" ++ systemToString bidDef.subsequentBids ++ ")" ++ systemToString rest
            

requirementsToString : List HandRange -> String
requirementsToString meanings =
    case meanings of
        [] -> ""
        {points , spades, hearts, diamonds, clubs} :: rest -> 
            let (minP, maxP) = points in
            let (minS, maxS) = spades in
            let (minH, maxH) = hearts in
            let (minD, maxD) = diamonds in
            let (minC, maxC) = clubs in
            "p" ++ String.fromInt minP ++ "-" ++ String.fromInt maxP ++
            "s" ++ String.fromInt minS ++ "-" ++ String.fromInt maxS ++
            "h" ++ String.fromInt minH ++ "-" ++ String.fromInt maxH ++
            "d" ++ String.fromInt minD ++ "-" ++ String.fromInt maxD ++
            "c" ++ String.fromInt minC ++ "-" ++ String.fromInt maxC ++ 
            case rest of
                 [] -> ""
                 _ ->
                      "|" ++ (requirementsToString rest)

stringToHandRange : String -> Maybe (HandRange)
stringToHandRange string =
    case List.filterMap String.toInt (String.split "!" (String.map (\c -> if Char.isDigit c then c else '!') string)) of
        [pMin, pMax, sMin, sMax, hMin, hMax, dMin, dMax, cMin, cMax] -> Just {points = (pMin, pMax),
                                                                            spades = (sMin, sMax),
                                                                            hearts = (hMin, hMax),
                                                                            diamonds = (dMin, dMax),
                                                                            clubs = (cMin, cMax)}
        _ -> Nothing

            
stringToSystem : String -> Maybe BiddingRules
stringToSystem systemString =
    case systemString of
        "" -> Just []
        _  -> case getParenIndex systemString 0 Nothing 0 of
            Nothing -> Nothing
            Just (lParen, rParen) -> let preParens = String.slice 0 lParen systemString in
                                     let betweenParens = String.slice (lParen+1) rParen systemString in
                                     let postParens = String.dropLeft (rParen+1) systemString in
                                     case (parsePreParens preParens, stringToSystem betweenParens, stringToSystem postParens) of
                                     (Just (reqs, value), Just between, Just post) -> Just ([BidDefinition {requirements = reqs, bidValue = value, subsequentBids = between}] ++ post)
                                     _ -> Nothing

stringToSystemList : String -> List BiddingRules
stringToSystemList string = 
    List.filterMap stringToSystem (String.split "Y" string)                                           
                                
parsePreParens : String -> Maybe (List HandRange, Bid)
parsePreParens string = 
    case String.split ":" string of
        valueString::(requirementsString::[]) -> case stringToBid valueString of
                                                    Just bid -> Just (List.filterMap stringToHandRange (String.split "|" requirementsString), bid)
                                                    Nothing -> Nothing
        _ -> Nothing


getParenIndex : String -> Int -> Maybe Int -> Int -> Maybe (Int, Int)
getParenIndex currentString index firstParen parenDepth =
    if currentString == "" then Nothing else
    if (parenDepth == 1) && (String.left 1 currentString == ")")
    then case firstParen of
        Just firstParenIndex -> Just (firstParenIndex, index)  
        Nothing -> Nothing
    else if (String.left 1 currentString == ")")
         then getParenIndex (String.dropLeft 1 currentString) (index+1) firstParen (parenDepth-1)
         else if (String.left 1 currentString == "(")
              then case firstParen of
                  Nothing -> getParenIndex (String.dropLeft 1 currentString) (index+1) (Just index) (parenDepth+1)
                  Just _ -> getParenIndex (String.dropLeft 1 currentString) (index+1) firstParen (parenDepth+1)
              else getParenIndex (String.dropLeft 1 currentString) (index+1) firstParen parenDepth
                      
parseWithNames : String -> String -> List (String, BiddingRules)
parseWithNames string digitString = case String.uncons string of
    Nothing -> []   
    Just ('L', rest) -> case String.toInt digitString of
        Nothing -> []
        Just nameLength -> let name = String.left nameLength rest in
                           let leftover = String.dropLeft nameLength rest in
                           let (systemString, nextSystem) = case String.indices "Y" leftover of
                                [] -> (leftover, "")
                                index::_ -> (String.left index leftover, String.dropLeft (index+1) leftover)
                           in case stringToSystem systemString of
                                Nothing -> []
                                Just system -> [(name, system)] ++ parseWithNames nextSystem ""
    Just (digit, rest) -> parseWithNames rest (String.cons digit digitString)

        

port saveSystems : String -> Cmd msg


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch []


-- MAIN

type alias Flags = Maybe String

init : Flags -> (Model, Cmd Msg)
init systems = (initModel systems, Cmd.none)

main : Program Flags Model Msg
main =
  Browser.element {init = init,
                   view = view,
                   update = update,
                   subscriptions = subscriptions}