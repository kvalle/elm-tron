module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard


type alias Model =
    { path : Path
    , direction : Direction
    , state : GameState
    }


type GameState
    = NotStarted
    | Running
    | GameOver


width : number
width =
    50


height : number
height =
    40


type alias Path =
    List Pos


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Pos =
    ( Float, Float )


type Msg
    = Tick Time.Time
    | KeyPress Keyboard.KeyCode


init : ( Model, Cmd Msg )
init =
    ( { path = [ ( 0, 0 ) ]
      , direction = Up
      , state = NotStarted
      }
    , Cmd.none
    )


move : Model -> Model
move model =
    let
        ( x, y ) =
            List.head model.path |> Maybe.withDefault ( 0, 0 )
    in
        { model
            | path =
                case model.direction of
                    Up ->
                        ( x, y + 1 ) :: model.path

                    Down ->
                        ( x, y - 1 ) :: model.path

                    Left ->
                        ( x - 1, y ) :: model.path

                    Right ->
                        ( x + 1, y ) :: model.path
        }


checkPosition : Model -> Model
checkPosition model =
    let
        (( x, y ) as head) =
            List.head model.path |> Maybe.withDefault ( 0, 0 )

        tail =
            List.tail model.path |> Maybe.withDefault []
    in
        { model
            | state =
                if abs x > (width / 2) then
                    GameOver
                else if abs y > (height / 2) then
                    GameOver
                else if List.member head tail then
                    GameOver
                else
                    Running
        }


changeDirection : Keyboard.KeyCode -> Model -> Model
changeDirection keyCode model =
    { model
        | direction =
            case keyCode of
                37 ->
                    Left

                38 ->
                    Up

                39 ->
                    Right

                40 ->
                    Down

                _ ->
                    model.direction
    }


setState : GameState -> Model -> Model
setState gameState model =
    { model | state = gameState }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( case model.state of
                Running ->
                    model
                        |> move
                        |> checkPosition

                _ ->
                    model
            , Cmd.none
            )

        KeyPress keyCode ->
            ( model
                |> setState Running
                |> changeDirection keyCode
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        pixelSize =
            10
    in
        div []
            [ h1 []
                [ text "Tron" ]
            , span []
                [ text <|
                    case model.state of
                        NotStarted ->
                            "Press SPACE to begin"

                        Running ->
                            "Race!"

                        GameOver ->
                            "Game over"
                ]
            , div [ class "board" ]
                [ let
                    snake =
                        if model.state == NotStarted then
                            Collage.square pixelSize
                                |> Collage.filled Color.black
                                |> Collage.move (List.head model.path |> Maybe.withDefault ( 0, 0 ))
                        else
                            model.path
                                |> List.map (Tuple.mapFirst <| (*) pixelSize)
                                |> List.map (Tuple.mapSecond <| (*) pixelSize)
                                |> Collage.path
                                |> Collage.traced
                                    { defaultLine
                                        | width = pixelSize
                                        , cap = Collage.Padded
                                    }
                  in
                    snake
                        |> List.singleton
                        |> Collage.collage (width * pixelSize + pixelSize // 2) (height * pixelSize + pixelSize // 2)
                        |> Element.toHtml
                ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running ->
            Sub.batch
                [ Time.every (Time.millisecond * 100) Tick
                , Keyboard.downs KeyPress
                ]

        NotStarted ->
            Keyboard.downs KeyPress

        _ ->
            Sub.none
