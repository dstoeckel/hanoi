module Hanoi exposing (..)

import Svg exposing (rect, svg)
import Svg.Attributes exposing (width, height, x, y, class, points, transform)
import Html exposing (div, button, text, input, label)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Browser exposing ( sandbox )


main =
  sandbox
    { init = init 4
    , view = view
    , update = update
    }


type Peg = Left | Right | Middle


type Msg = Reset | Select Peg | NumRings Int


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


makeRing numRing peg numRingOnPeg pos id =
  let
    w =
      (ringMaxWidth * id) // numRing

    shift =
      pegHeight numRing - (numRingOnPeg - pos) * ringHeight + selectionHeight
  in
    rect
      [ (String.fromInt >> height) ringHeight
      , (String.fromInt >> x) (pegPosition peg + (pegWidth - w) // 2)
      , (String.fromInt >> y) shift
      , (String.fromInt >> width) w
      , class ("ring ring" ++ String.fromInt id)
      , onClick (Select peg)
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


makePeg p numRings =
  rect
    [ class "peg"
    , (String.fromInt >> width) pegWidth
    , (pegHeight >> String.fromInt >> height) numRings
    , (String.fromInt >> x) (pegPosition p)
    , (String.fromInt >> y) selectionHeight
    , onClick (Select p)
    ] []


spacing = 20

baseWidth =
  4 * spacing + 3 * ringMaxWidth


baseHeight = 20

base numRings =
  rect
    [ (String.fromInt >> width) baseWidth
    , (String.fromInt >> height) baseHeight
    , (String.fromInt >> y) (pegHeight numRings + selectionHeight)
    , class "base"
    ] []


-- Draw an arrow above the given peg
arrow : Peg -> Svg.Svg msg
arrow p =
  Svg.polygon
    [ points "5,15 10,8 7,8 7,0 3,0 3,8 0,8"
    , transform ("translate(" ++ String.fromInt (pegPosition p) ++ ",0)")
    ] []


-- Draw a peg an all its rings
drawPeg numRings selection p rings =
  let
    drawRing =
      makeRing numRings p (List.length rings)

    selectionIndicator =
      if isSelected p selection then
        [ arrow p ]
      else
        []

    pegs =
      [ makePeg p numRings ]

    items =
      List.indexedMap drawRing rings
  in
    Svg.g
      [ class "pegGroup", width (String.fromInt ringMaxWidth) ]
      (List.concat [ pegs, items, selectionIndicator ])


isSelected : Peg -> Maybe Peg -> Bool
isSelected peg sel =
  case sel of
    Nothing ->
      False

    Just a ->
      peg == a


board model =
  let
    f =
      drawPeg model.rings model.selected
  in
    svg
      [ (String.fromInt >> width) baseWidth
      , (String.fromInt >> height) (baseHeight + pegHeight model.rings + selectionHeight)
      ]
      [ base model.rings
      , f Left model.left
      , f Middle model.middle
      , f Right model.right
      ]


inputNumRings: Int -> String -> Msg
inputNumRings numRings x =
  let
    decoded =
      Maybe.withDefault numRings (String.toInt x)
  in
    NumRings (max minNumRings (min maxNumRings decoded))


minNumRings = 1
maxNumRings = 6


ringInput: Int -> Html.Html Msg
ringInput numRings =
  input
    [ Html.Attributes.type_ "number"
    , Html.Attributes.min "1"
    , Html.Attributes.max "6"
    , Html.Attributes.value (String.fromInt numRings)
    , onInput (inputNumRings numRings)
    ] []


view model =
  div []
    [ button [ onClick Reset ] [ text "Reset" ]
    , text " "
    , label [] [ text "Rings: ", ringInput model.rings ]
    , div [] [ board model ]
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
    y :: ys -> ys


allowed : Model -> Int -> Maybe Int -> Bool
allowed model from to =
  case to of
    Nothing ->
      True

    Just idx ->
      from < idx


update: Msg -> Model -> Model
update msg model =
  case msg of
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
move oldModel from to =
  let
    fromRing =
      topRing oldModel from

    toRing =
      topRing oldModel to

    model =
      { oldModel | selected = Nothing }
  in
    case fromRing of
      Just ring ->
        if allowed model ring toRing then
          (pop from >> push to ring) model
        else
          model

      Nothing ->
        model
