module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, text, img, br, h1)
import Html.Attributes exposing (src, width, height, style)
import Html.Events exposing (onClick)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL
type GridElement = Empty | Cross | Circle
type Turn = Circles | Crosses 
type Winner = CircleWinner | CrossWinner | NoWinner
type alias GridElements = Array GridElement
type alias Model = { turn: Turn, winner : Winner, grid : GridElements }
type alias WinningPositions = List (List Int)

init : Model
init = Model Circles NoWinner (Array.fromList 
                        ([Empty, Empty, Empty,
                          Empty, Empty, Empty,
                          Empty, Empty, Empty ]))

winningPositions : WinningPositions
winningPositions = [[1, 5, 9], [3, 5, 7], --diagonals
                    [1, 2, 3], [4, 5, 6], [7, 8, 9], -- horizontal
                    [1, 4, 7], [2, 5, 8], [3, 6, 9]] -- vertical

-- UPDATE
type Msg
  = Click Int
  | Reset

gridElementEmpty:  Int -> GridElements -> Bool
gridElementEmpty i grid =
  (Array.get i grid) == Just Empty 

checkWon: GridElement -> Winner -> GridElements -> Winner
checkWon element winner grid = 
  let posList = List.map (\tuple -> (Tuple.first tuple)) --get index from tuple
                <| List.filter (\tuple -> (Tuple.second tuple) == element) -- get only X, O or empty
                <| List.indexedMap (\i e -> (i + 1, e)) (Array.toList grid) -- make tuple (+1 because we start from 1)

  -- filter the intList by removing anything that is not in the winning list then check if == 3                         
  in
     -- test if any list in winningPosition is contained in our list      
     if List.any (\winningList ->  List.length (List.filter (\e -> List.member e winningList) posList) == 3) winningPositions then 
        winner
     else 
        NoWinner   
update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset ->
      init
    Click i ->
      let isSlotEmpty = (gridElementEmpty i model.grid)
          circleTurn =  model.turn == Circles
          hasAnybodyWon = (model.winner == CircleWinner || model.winner == CrossWinner)
      in
        if circleTurn && isSlotEmpty && not hasAnybodyWon then
          model 
                |> (\m -> {m | turn = Crosses, grid = (Array.set i Circle m.grid)}) --update the grid
                |> (\m -> {m | winner = (checkWon Circle CircleWinner m.grid)})     --update the winner

        else if isSlotEmpty && not hasAnybodyWon then
           model 
                |> (\m -> {m | turn = Circles, grid = (Array.set i Cross m.grid)}) 
                |> (\m -> {m | winner = (checkWon Cross CrossWinner m.grid)})
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

gridFull grid = 
  List.all (\e -> e /= Empty) (Array.toList grid)

winnerToString winner grid  =
  let gridfull = (gridFull grid)
  in
   "Winner : " ++
    if not gridfull then
      case winner of
        NoWinner -> ""
        CircleWinner -> "Circles"
        CrossWinner -> "Crosses"
    else if gridfull then
      case winner of
        NoWinner -> "No Winner"
        CircleWinner -> "Circles"
        CrossWinner -> "Crosses"
    else 
      "No Winner"        

view : Model -> Html Msg
view model =
  div [style "position" "absolute", style "top" "50%", style "left" "50%", style "margin-right" "-50%", style "transform" "translate(-50%, -50%)"]  
  (List.concat
    [
      [
        h1 [] [ text("Tic Tac Toe") ]
      ],
        (List.concat 
          <| List.indexedMap drawGrid 
          <| Array.toList model.grid),
      [
        div [] [ text ("Turn : " ++ (turnToString model.turn))],
        div [] [ text (winnerToString model.winner model.grid)],
        button [ onClick Reset ] [ text "Reset" ]
      ]
    ])