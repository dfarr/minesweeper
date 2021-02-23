module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, a, button, div, h1, input, table, tr, td, text, span, strong)
import Html.Attributes exposing (attribute, href, style, type_, value)
import Html.Events exposing (custom, onBlur, onClick, onInput, onMouseDown, onMouseUp)
import Json.Decode as Decode exposing (Decoder)
import Random
import Set exposing (Set)

import Grid exposing (Grid, Coord)



-- MAIN

main =
  Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- MODEL

type alias Model =
  { index : Int
  , mouse : Bool
  , mines : Set Int
  , board : ( Board Int, Board String )
  , state : { initial : State, current : State }
  , events : List Event
  }

type State
  = Initial Coord (Grid Square)
  | Pending (Grid Square)
  | Success (Grid Square)
  | Failure (Grid Square)

type alias Board a =
  { rows : a, cols : a, mines : a }

type Square
  = Hidden Value
  | Marked Value
  | Exposed Value

type Value
  = Safe Int
  | Mine


-- INIT

initBoard : Board Int
initBoard =
  { rows = 16, cols = 16, mines = 40 }

initModel : Board Int -> Model
initModel board =
  let
    initial =
      Hidden (Safe 0)
        |> Grid.repeat board.rows board.cols
        |> Initial ( 0, 0 )
    board_ =
      { rows = board.rows |> String.fromInt
      , cols = board.cols |> String.fromInt
      , mines = board.mines |> String.fromInt
      }
  in
  { index = 0
  , mouse = False
  , mines = Set.empty
  , board = ( board, board_ )
  , state = { initial = initial, current = initial }
  , events = []
  }

initCommand : Board Int -> Cmd Msg
initCommand { rows, cols } =
  Random.int 0 (rows * cols - 1)
    |> Random.generate Setup

init : () -> ( Model, Cmd Msg )
init _ =
  ( initModel initBoard, initCommand initBoard )


-- UPDATE

type Msg
  = NoOp
  | Reset
  | Setup Int
  | Mouse Bool
  | Click Event
  | Replay Int
  | SetBoard (Board String)

type Event
  = Clicked Coord Square
  | Flagged Coord Square
  | Special Coord Square

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
  let
    { index, mines, board, state, events } = model
    ( intBoard, strBoard ) = board
  in
  case ( message, state.current ) of
    ( Reset, _ ) ->
      let
        rows =
          strBoard.rows
            |> String.toInt
            |> Maybe.withDefault intBoard.rows
            |> max 1
        cols =
          strBoard.cols
            |> String.toInt
            |> Maybe.withDefault intBoard.cols
            |> max 1
        mines_ =
          strBoard.mines
            |> String.toInt
            |> Maybe.withDefault intBoard.mines
            |> max 0
            |> min (rows * cols - 1)
        board_ =
          { rows = rows, cols = cols, mines = mines_ }
      in
      ( initModel board_, initCommand board_ )

    ( Setup value, Initial _ grid ) ->
      let
        mines_ =
          Set.insert value mines
        ( current, command ) =
          case Set.size mines_ == intBoard.mines + 1 of
            True ->
              ( mines_
                |> Set.toList
                |> List.map (Grid.indexToCoord intBoard.rows)
                |> List.foldl (Grid.map toMine increment identity) grid
                |> Initial (value |> Grid.indexToCoord intBoard.rows)
              , Cmd.none )
            False ->
              ( state.current, initCommand intBoard )
      in
      ( { model | mines = mines_, state = { initial = current, current = current } }, command )

    ( Click event, _ ) ->
      let
        current =
          apply event state.current
        ( index_, events_ ) =
          case current == state.current of
            True -> ( index, events )
            False -> ( index + 1, [ event ] |> (++) (List.take index events) )
      in
      ( { model | index = index_, state = { state | current = current }, events = events_ }, Cmd.none )

    ( Mouse value, _ ) ->
      ( { model | mouse = value }, Cmd.none )

    ( Replay value, _ ) ->
      let
        current =
          events
            |> List.take value
            |> List.foldl apply state.initial
      in
      ( { model | index = value, state = { state | current = current } }, Cmd.none )

    ( SetBoard board_, _ ) ->
      ( { model | board = ( intBoard, board_ ) }, Cmd.none )

    _ ->
      ( model, Cmd.none )


-- APPLY

apply : Event -> State -> State
apply event state =
  case ( state, event ) of
    ( Initial mine grid, Clicked coord (Hidden value) ) ->
      let
        coord_ =
          case value of
            Mine -> coord
            Safe _ -> mine
        square1 =
          grid
            |> Grid.map (always False) isMine (always False) coord_
            |> List.concat
            |> List.filter identity
            |> List.length
            |> Hidden << Safe
        square2 =
          Hidden value
            |> Grid.mapRelationship (always square1) decrement identity coord coord_
      in
      grid
        |> Grid.map (always square1) decrement identity coord_
        |> Pending
        |> apply (Clicked coord square2)

    ( Initial mine grid, Flagged coord (Hidden value) ) ->
      grid
        |> Grid.mapIdentity (Marked value) coord
        |> Initial mine

    ( Initial mine grid, Flagged coord (Marked value) ) ->
      grid
        |> Grid.mapIdentity (Hidden value) coord
        |> Initial mine

    ( Pending grid, Clicked coord (Hidden (Safe 0)) ) ->
      grid
        |> clickNeighbors coord
        |> List.foldl apply (grid |> Grid.mapIdentity (Safe 0 |> Exposed) coord |> check)

    ( Pending grid, Clicked coord (Hidden Mine) ) ->
      grid
        |> Grid.mapIdentity (Exposed Mine) coord
        |> Failure

    ( Pending grid, Clicked coord (Hidden value) ) ->
      grid
        |> Grid.mapIdentity (Exposed value) coord
        |> check

    ( Pending grid, Flagged coord (Hidden value) ) ->
      grid
        |> Grid.mapIdentity (Marked value) coord
        |> Pending

    ( Pending grid, Flagged coord (Marked value) ) ->
      grid
        |> Grid.mapIdentity (Hidden value) coord
        |> Pending

    ( Pending grid, Special coord (Exposed (Safe value)) ) ->
      let
        flagged =
          grid
            |> Grid.map (always False) marked (always False) coord
            |> List.concat
            |> List.filter identity
            |> List.length
      in
      case flagged == value of
        True ->
          grid
            |> clickNeighbors coord
            |> List.foldl apply (Pending grid)
        False ->
          grid |> Pending

    _ -> state

mapValue : (Int -> Int) -> Square -> Square
mapValue f square =
  case square of
    Hidden (Safe value) ->
      f value |> Safe |> Hidden
    Marked (Safe value) ->
      f value |> Safe |> Marked
    _ ->
      square

increment : Square -> Square
increment =
  mapValue <| (+) 1

decrement : Square -> Square
decrement =
  mapValue <| (+) -1

clickNeighbors : Coord -> Grid Square -> List Event
clickNeighbors coord =
  Grid.indexedMap (always Nothing) click (always Nothing) coord
    >> List.concat
    >> List.filterMap identity

click : ( Coord, Square ) -> Maybe Event
click ( coord, square ) =
  case square of
    Hidden _ -> Just (Clicked coord square)
    _ -> Nothing

check : Grid Square -> State
check grid =
  case grid |> List.concat |> List.all found of
    True -> Success grid
    False -> Pending grid

found : Square -> Bool
found square =
  case square of
    Hidden Mine -> True
    Marked Mine -> True
    Exposed (Safe _) -> True
    _ -> False

hidden : Square -> Bool
hidden square =
  case square of
    Hidden _ -> True
    _ -> False

marked : Square -> Bool
marked square =
  case square of
    Marked _ -> True
    _ -> False

exposed : Square -> Bool
exposed square =
  case square of
    Exposed _ -> True
    _ -> False

isMine : Square -> Bool
isMine square =
  case square of
    Hidden Mine -> True
    Marked Mine -> True
    _ -> False

toMine : Square -> Square
toMine square =
  case square of
    Marked _ -> Marked Mine
    _ -> Hidden Mine


-- VIEW

view : Model -> Document Msg
view model =
  { title = "Minesweeper"
  , body = [ render model ]
  }

render : Model -> Html Msg
render { index, mouse, board, state, events } =
  let
    grid_ =
      case state.current of
        Initial _ grid -> grid
        Pending grid -> grid
        Success grid -> grid
        Failure grid -> grid
    emoji =
      case ( state.current, mouse ) of
        ( Initial _ _, True ) -> "😮"
        ( Initial _ _, False ) -> "🙂"
        ( Pending _, True ) -> "😮"
        ( Pending _, False ) -> "🙂"
        ( Success _, _ ) -> "😎"
        ( Failure _, _ ) -> "😵"
  in
  div []
    [ renderGrid grid_ emoji
    , renderFoot (board |> Tuple.second) grid_ index (List.length events)
    ]

renderGrid : Grid Square -> String -> Html Msg
renderGrid grid emoji =
  div []
    [ h1 
      [ style "text-align" "center"
      , style "font-size" "48px"
      , onClick Reset
      ]
      [ text emoji ]
    , table tableStyle
        (grid
          |> List.indexedMap (List.indexedMap << Grid.withCoord renderSquare)
          |> List.map (tr trStyle))
    ]

renderSquare : Coord -> Square -> Html Msg
renderSquare coord square =
  case square of
    Hidden _ ->
      td tdStyle
        [ hiddenButtonAttributes coord square |> renderButton "" "" ]
    Marked _ ->
      td tdStyle
        [ hiddenButtonAttributes coord square |> renderButton "" "🚩" ]
    Exposed Mine ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "" "💣" ]
    Exposed (Safe 1) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "blue" "1" ]
    Exposed (Safe 2) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "green" "2" ]
    Exposed (Safe 3) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "red" "3" ]
    Exposed (Safe 4) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "darkblue" "4" ]
    Exposed (Safe 5) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "maroon" "5" ]
    Exposed (Safe 6) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "darkcyan" "6" ]
    Exposed (Safe 7) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "purple" "7" ]
    Exposed (Safe 8) ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "grey" "8" ]
    _ ->
      td tdStyle
        [ exposedButtonAttributes coord square |> renderButton "" "" ]

renderButton :  String -> String -> List (Html.Attribute Msg) -> Html Msg
renderButton color text_ attributes =
  button
    attributes
    [ strong [ style "color" color ] [ text text_ ] ]

renderFoot : Board String -> Grid Square -> Int -> Int -> Html Msg
renderFoot board grid value max =
  div
    [ style "bottom" "0"
    , style "position" "fixed"
    , style "text-align" "center"
    , style "width" "100%"
    , style "font-family" "monospace"
    , style "background" "rgb(239, 239, 239)"
    , style "border-top" "1px solid rgb(169, 169, 169)"
    ]
    [ table [ style "width" "100%", style "padding" "10px 15px" ]
      [ tr []
        [ td [ style "width" "20%" ] [ renderInputLeft board ]
        , td [ style "width" "60%" ] [ renderSlider value max ]
        , td [ style "width" "20%" ] [ renderInputRight board ]
        ]
      ]
    , table
      [ style "width" "100%"
      , style "padding" "5px 15px"
      , style "font-size" "14px"
      , style "background" "rgb(219, 219, 219)"
      ]
      [ tr []
        [ td [ style "text-align" "left" ] [ renderStatsLeft (grid |> List.concat) value max ]
        , td [ style "text-align" "right" ] [ renderStatsRight ]
        ]
      ]
    ]

renderInputLeft : Board String -> Html Msg
renderInputLeft board =
  span []
    [ input
        [ type_ "text"
        , style "color" "rgb(20, 20, 20)"
        , style "text-align" "center"
        , style "font-size" "20px"
        , style "background" "transparent"
        , style "border-width" "0px 0px 2px 0px"
        , style "border-color" "rgb(20, 20, 20)"
        , style "border-radius" "0px"
        , style "width" "36px"
        , value board.cols
        , onInput (\cols -> SetBoard { board | cols = cols })
        , onBlur Reset
        ] []
    , span [ style "font-size" "18px" ] [ text " ✖️ " ]
    , input
        [ type_ "text"
        , style "color" "rgb(20, 20, 20)"
        , style "text-align" "center"
        , style "font-size" "20px"
        , style "background" "transparent"
        , style "border-width" "0px 0px 2px 0px"
        , style "border-color" "rgb(20, 20, 20)"
        , style "border-radius" "0px"
        , style "width" "36px"
        , value board.rows
        , onInput (\rows -> SetBoard { board | rows = rows })
        , onBlur Reset
        ] []
    ]

renderSlider : Int -> Int -> Html Msg
renderSlider value_ max =
  input
    [ type_ "range"
    , Html.Attributes.min "0"
    , Html.Attributes.max (max |> String.fromInt)
    , value (value_ |> String.fromInt)
    , style "-webkit-appearance" "none"
    , style "background" "rgb(219, 219, 219)"
    , style "border" "2px solid rgb(169, 169, 169)"
    , style "border-radius" "20px"
    , style "width" "100%"
    , style "height" "20px"
    , onInput (Replay << Maybe.withDefault value_ << String.toInt)
    ] []

renderInputRight : Board String -> Html Msg
renderInputRight board =
  span []
    [ input
        [ type_ "text"
        , style "color" "rgb(20, 20, 20)"
        , style "text-align" "center"
        , style "font-size" "20px"
        , style "background" "transparent"
        , style "border-width" "0px 0px 2px 0px"
        , style "border-color" "rgb(20, 20, 20)"
        , style "border-radius" "0px"
        , style "width" "36px"
        , value board.mines
        , onInput (\mines -> SetBoard { board | mines = mines })
        , onBlur Reset
        ] []
    , span [ style "font-size" "18px" ] [ text " 💣 " ]
    ]

renderStatsLeft : List Square -> Int -> Int -> Html Msg
renderStatsLeft squares index events =
  let
    hidden_ =
      squares
        |> List.filter hidden
        |> List.length
        |> String.fromInt
    exposed_ =
      squares
        |> List.filter exposed
        |> List.length
        |> String.fromInt
    flagged_ =
      squares
        |> List.filter marked
        |> List.length
        |> String.fromInt
  in
  text
    ("event = " ++ (String.fromInt index) ++ " of " ++ (String.fromInt events)
      ++ " | hidden = " ++ hidden_
      ++ " | exposed = " ++ exposed_
      ++ " | flagged = " ++ flagged_)

renderStatsRight : Html Msg
renderStatsRight =
  a [ href "https://github.com/dfarr/minesweeper" ] [ text "dfarr/minesweeper" ]


-- STYLE

tableStyle : List (Html.Attribute Msg)
tableStyle =
  [ style "margin" "0 auto 120px auto"
  , attribute "cellpadding" "0"
  , attribute "cellspacing" "0"
  , attribute "border" "1"
  , onRightClick NoOp
  ]

trStyle : List (Html.Attribute Msg)
trStyle =
  [ style "padding" "0px"
  , style "margin" "0px"
  ]

tdStyle : List (Html.Attribute Msg)
tdStyle =
  [ style "padding" "0px"
  , style "margin" "0px"
  , style "width" "35px"
  , style "height" "35px"
  ]

buttonStyle : List (Html.Attribute Msg)
buttonStyle =
  [ style "font-size" "18px"
  , style "padding" "0px"
  , style "margin" "0px"
  , style "border" "0px"
  , style "width" "100%"
  , style "height" "100%"
  ]

hiddenButtonStyle : List (Html.Attribute Msg)
hiddenButtonStyle =
  buttonStyle ++
    [ style "background" "rgb(239, 239, 239)" ]

exposedButtonStyle : List (Html.Attribute Msg)
exposedButtonStyle =
  buttonStyle ++
    [ style "background" "rgb(219, 219, 219)" ]

buttonActions : List (Html.Attribute Msg)
buttonActions =
  [ onMouseDown (Mouse True)
  , onMouseUp (Mouse False)
  ]

hiddenButtonActions : Coord -> Square  -> List (Html.Attribute Msg)
hiddenButtonActions coord square =
  buttonActions ++
    [ onClick (Clicked coord square |> Click)
    , onRightClick (Flagged coord square |> Click)
    ]

exposedButtonActions : Coord -> Square  -> List (Html.Attribute Msg)
exposedButtonActions coord square =
  buttonActions ++
    [ onRightClick (Special coord square |> Click) ]

hiddenButtonAttributes : Coord -> Square  -> List (Html.Attribute Msg)
hiddenButtonAttributes coord square =
  hiddenButtonActions coord square
    ++ hiddenButtonStyle

exposedButtonAttributes : Coord -> Square  -> List (Html.Attribute Msg)
exposedButtonAttributes coord square =
  exposedButtonActions coord square
    ++ exposedButtonStyle


-- CLICKS

type alias MsgWithOptions =
  { message: Msg
  , preventDefault: Bool
  , stopPropagation: Bool
  }

onRightClick : Msg -> Html.Attribute Msg
onRightClick message =
  custom "contextmenu" (msgWithOptionsDecoder message)

msgWithOptionsDecoder : Msg -> Decoder MsgWithOptions
msgWithOptionsDecoder message =
  Decode.map (toMsgWithOptions message) (Decode.field "button" Decode.int)

toMsgWithOptions : Msg -> Int -> MsgWithOptions
toMsgWithOptions message id =
    case id of
      2 -> { message = message, preventDefault = True, stopPropagation = True }
      _ -> { message = NoOp, preventDefault = False, stopPropagation = False }
