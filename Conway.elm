module Conway where
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Set
import Time
import Window


-- MODEL

type alias Cell = (Int, Int)


-- The model is a simple list of the currently live cells.
type alias Model = List Cell


-- Start with an interesting initial board.
initialModel : Model
initialModel = [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 3), (-1, 3)]


neighbours : Cell -> List Cell
neighbours (x, y) =
  [(x - 1, y - 1)
  ,(x - 1, y    )
  ,(x - 1, y + 1)
  ,(x    , y - 1)
  ,(x    , y + 1)
  ,(x + 1, y - 1)
  ,(x + 1, y    )
  ,(x + 1, y + 1)
  ]


combinedNeighbours : List Cell -> List Cell
combinedNeighbours cells =
  cells
  |> List.concatMap neighbours
  |> Set.fromList
  |> Set.toList


countLiveNeighbours : List Cell -> Cell -> Int
countLiveNeighbours liveCells cell =
  let
    isLive cell = List.member cell liveCells
    liveNeighbours = List.filter isLive (neighbours cell)
  in
    List.length liveNeighbours


willSurvive : List Cell -> Cell -> Bool
willSurvive liveCells cell =
  let
    count = countLiveNeighbours liveCells cell
  in
    count == 2 || count == 3


willSpawn : List Cell -> Cell -> Bool
willSpawn liveCells cell =
  countLiveNeighbours liveCells cell == 3


-- Update from one generation of live cells to the next, following
-- the rules at: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
--
-- Specifically:
-- * Any live cell with two or three live neighbours lives on to the next generation.
-- * Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
--
step : List Cell -> List Cell
step liveCells =
  let
    survivors = List.filter (willSurvive liveCells) liveCells
    spawners = List.filter (willSpawn liveCells) (combinedNeighbours liveCells)
  in
    survivors ++ spawners


-- UPDATE

type Action
  = Step


update : Action -> Model -> Model
update action model =
  case action of
    Step ->
      step model


model : Signal Model
model =
  Signal.foldp update initialModel inputs

-- SIGNALS

inputs : Signal Action
inputs =
  Signal.map (always Step) (Time.every 300)

-- VIEW


view : (Int, Int) -> Model -> Element
view (w, h) model =
  let
    (w', h') = (toFloat w, toFloat h)
    cellForms = List.map (drawCell (w, h)) model
    background = filled black (rect w' h')
    debug = toForm (show (w, h))
  in
    collage w h ([background, debug] ++ cellForms)


drawCell : (Int, Int) -> Cell -> Form
drawCell (w, h) (x, y) =
  let
    (x', y') = (toFloat x, toFloat y)
    size = (toFloat h)/50
    position = (x'*size, y'*size)
    shape = filled green (circle (size/2))
  in
    move position shape


-- GLUE

main : Signal Element
main =
  Signal.map2 view Window.dimensions model