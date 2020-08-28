module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text, img, br)
import Html.Attributes exposing (src, width, height, style)
import Html.Events exposing (onClick)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type GridElement = Empty | Cross | Circle
type Turn = Circles | Crosses 
type alias GridElements = Array GridElement
type alias Model = { turn: Turn, grid : GridElements }

init : Model
init = Model Circles (Array.fromList 
                        ([Empty, Empty, Empty,
                          Empty, Empty, Empty,
                          Empty, Empty, Empty ]))

-- UPDATE
type Msg
  = Click Int
  | Reset

gridElementEmpty:  Int -> GridElements -> Bool
gridElementEmpty i grid =
  (Array.get i grid) == Just Empty 
       

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
      init
    Click i ->
      if model.turn == Circles && (gridElementEmpty i model.grid) then
        {model  | turn = Crosses, grid = (Array.set i Circle model.grid) }
      else if (gridElementEmpty i model.grid) then
        {model  | turn = Circles, grid = (Array.set i Cross model.grid) }
      else 
        model

-- VIEW
gridElementToString: GridElement -> String
gridElementToString el =
  case el of 
    Circle -> "circle.png"
    Cross  -> "cross.png"
    Empty  -> "empty.png" 

drawGrid : Int -> GridElement -> List (Html Msg)
drawGrid i el = 
  let elementToPrint = img [src (gridElementToString el), 
          (style "border" "solid 1px"), 
          width 100, height 100, 
          onClick (Click i)] [] 
  in       
    if (remainderBy 3 (i+1)) == 0 then 
        [elementToPrint, br [] []]
    else
        [ elementToPrint ]  

turnToString turn = 
  case turn of
    Circles -> "Circles"
    Crosses -> "Crosses" 

view : Model -> Html Msg
view model =
  div []  
  (List.append
      (List.concat 
        <| List.indexedMap drawGrid 
        <| Array.toList model.grid)
    [
      div [] [ text ("Turn : " ++ (turnToString model.turn))],
      button [ onClick Reset ] [ text "Reset" ]
    ])