module Hanoi (..) where

import String
import Svg exposing (rect, svg)
import Svg.Attributes exposing (width, height, x, y, class, points, transform)
import Html exposing (div, button, text, input, label)
import Html.Attributes
import Html.Events exposing (onClick, on)
import StartApp.Simple as StartApp


main =
  StartApp.start { model = init 4, view = view, update = update }


type Peg = Left | Right | Middle


type Action = Reset | Select Peg | NumRings Int


type alias Model =
  { rings : Int
  , left : List Int
  , right : List Int
  , middle : List Int
  , selected : Maybe Peg
  }


seq_help : Int -> Int -> List Int -> List Int
seq_help from to xs =
  if from > to then
    xs
  else
    seq_help from (to - 1) (to :: xs)


seq : Int -> Int -> List Int
seq from to =
  seq_help from to []


init : Int -> Model
init n =
  { rings = n
  , left = seq 1 n
  , right = []
  , middle = []
  , selected = Nothing
  }


ringHeight = 20
ringMinWidth = 25
ringMaxWidth = 100


ring address numRing peg numRingOnPeg pos id =
  let
    w =
      (ringMaxWidth * id) // numRing

    shift =
      pegHeight numRing - (numRingOnPeg - pos) * ringHeight + selectionHeight
  in
    rect
      [ (toString >> height) ringHeight
      , (toString >> x) (pegPosition peg + (pegWidth - w) // 2)
      , (toString >> y) shift
      , (toString >> width) w
      , class ("ring ring" ++ toString id)
      , onClick address (Select peg)
      ] []


selectionHeight = 20
pegWidth = 10


pegHeight numRings =
  (numRings + 1) * ringHeight


pegPosition peg =
  spacing
    + (ringMaxWidth - pegWidth)
    // 2
    + (index peg - 1)
    * (ringMaxWidth + spacing)


peg address p numRings =
  rect
    [ class "peg"
    , (toString >> width) pegWidth
    , (pegHeight >> toString >> height) numRings
    , (toString >> x) (pegPosition p)
    , (toString >> y) selectionHeight
    , onClick address (Select p)
    ] []


spacing = 20

baseWidth =
  4 * spacing + 3 * ringMaxWidth


baseHeight = 20

base numRings =
  rect
    [ (toString >> width) baseWidth
    , (toString >> height) baseHeight
    , (toString >> y) (pegHeight numRings + selectionHeight)
    , class "base"
    ] []



-- Draw an arrow above the given peg
arrow : Peg -> Svg.Svg
arrow p =
  Svg.polygon
    [ points "5,15 10,8 7,8 7,0 3,0 3,8 0,8"
    , transform ("translate(" ++ toString (pegPosition p) ++ ",0)")
    ] []


-- Draw a peg an all its rings
-- This needs the update-address for connecting the onClick event
drawPeg address numRings selection p rings =
  let
    drawRing =
      ring address numRings p (List.length rings)

    selectionIndicator =
      if isSelected p selection then
        [ arrow p ]
      else
        []

    pegs =
      [ peg address p numRings ]

    items =
      List.indexedMap drawRing rings
  in
    Svg.g
      [ class "pegGroup", width (toString ringMaxWidth) ]
      (List.concat [ pegs, items, selectionIndicator ])


isSelected : Peg -> Maybe Peg -> Bool
isSelected peg sel =
  case sel of
    Nothing ->
      False

    Just a ->
      peg == a


board address model =
  let
    f =
      drawPeg address model.rings model.selected
  in
    svg
      [ (toString >> width) baseWidth
      , (toString >> height) (baseHeight + pegHeight model.rings + selectionHeight)
      ]
      [ base model.rings
      , f Left model.left
      , f Middle model.middle
      , f Right model.right
      ]


inputNumRings address numRings x =
  let
    decoded =
      Result.withDefault numRings (String.toInt x)

    n =
      (max minNumRings (min maxNumRings decoded))
  in
    Signal.message address (NumRings n)


minNumRings = 1
maxNumRings = 6


ringInput numRings address =
  input
    [ Html.Attributes.type' "number"
    , Html.Attributes.min "1"
    , Html.Attributes.max "6"
    , Html.Attributes.value (toString numRings)
    , on "change" Html.Events.targetValue (inputNumRings address numRings)
    ] []


view address model =
  div []
    [ button [ onClick address Reset ] [ text "Neu" ]
    , text " "
    , label [] [ text "Ringe: ", ringInput model.rings address ]
    , div [] [ board address model ]
    ]


index : Peg -> Int
index p =
  case p of
    Left   -> 1
    Middle -> 2
    Right  -> 3


getPeg : Model -> Peg -> List Int
getPeg model peg =
  case peg of
    Left ->
      model.left

    Middle ->
      model.middle

    Right ->
      model.right


topRing : Model -> Peg -> Maybe Int
topRing model peg =
  (getPeg model >> List.head) peg


push : Peg -> Int -> Model -> Model
push peg ring model =
  case peg of
    Left ->
      { model | left = ring :: model.left }

    Middle ->
      { model | middle = ring :: model.middle }

    Right ->
      { model | right = ring :: model.right }


pop : Peg -> Model -> Model
pop peg model =
  case peg of
    Left ->
      { model | left = popList model.left }

    Middle ->
      { model | middle = popList model.middle }

    Right ->
      { model | right = popList model.right }


popList : List a -> List a
popList xs =
  case xs of
    [] -> []
    x :: xs -> xs


allowed : Model -> Int -> Maybe Int -> Bool
allowed model from to =
  case to of
    Nothing ->
      True

    Just to ->
      from < to


update action model =
  case action of
    Reset ->
      init model.rings

    NumRings numRings ->
      init numRings

    Select peg ->
      case model.selected of
        Nothing ->
          if topRing model peg == Nothing then
            model
          else
            { model | selected = Just peg }

        Just from ->
          move model from peg


move : Model -> Peg -> Peg -> Model
move model from to =
  let
    fromRing =
      topRing model from

    toRing =
      topRing model to

    model =
      { model | selected = Nothing }
  in
    case fromRing of
      Just ring ->
        if allowed model ring toRing then
          (pop from >> push to ring) model
        else
          model

      Nothing ->
        model
