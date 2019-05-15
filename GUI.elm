module GUI exposing (main)

import Deal exposing (Card, shuffle, separate, handRecords, newDeal)
import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = {nextSeed : Int,
                    hands : List (List Card)}

initModel = {nextSeed = 0,
             hands = [[],[],[],[]]}


-- UPDATE

type Msg = Redeal

update : Msg -> Model -> Model
update msg model =
  let {nextSeed, hands} = model in
  case msg of
    Redeal ->
      let newHands = handRecords (separate (shuffle nextSeed))
-- this is NOT a good method
          newSeed = nextSeed + 1 in
      {nextSeed = newSeed, hands = newHands}


-- VIEW

view : Model -> Html Msg
view model =
  let {nextSeed, hands} = model in
  let redeal = Html.button [onClick Redeal] [Html.text "Redeal"]
      display = Html.text (Deal.newDeal nextSeed) in
  Html.div [] [redeal, display]


-- MAIN

main : Program () Model Msg
main =
  Browser.sandbox {init = initModel,
                   view = view,
                   update = update}