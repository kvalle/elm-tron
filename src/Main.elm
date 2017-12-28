module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Collage exposing (defaultLine)
import Element
import Time exposing (Time)
import Keyboard


type alias Model =
    { path : Path
    , direction : Direction
    , state : GameState
    }


type GameState
    = Running
    | GameOver


width : number
width =
    300


height : number
height =
    200


pixelSize : number
pixelSize =
    10


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
    ( { path = [ ( 0, pixelSize ) ]
      , direction = Up
      , state = Running
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
                        ( x, y + pixelSize ) :: model.path

                    Down ->
                        ( x, y - pixelSize ) :: model.path

                    Left ->
                        ( x - pixelSize, y ) :: model.path

                    Right ->
                        ( x + pixelSize, y ) :: model.path
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( case model.state of
                Running ->
                    model
                        |> move
                        |> checkPosition

                GameOver ->
                    model
            , Cmd.none
            )

        KeyPress keyCode ->
            ( changeDirection keyCode model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text "Tron" ]
        , span []
            [ text <|
                case model.state of
                    Running ->
                        "Race!"

                    GameOver ->
                        "Game over"
            ]
        , div [ class "board" ]
            [ Collage.path model.path
                |> Collage.traced
                    { defaultLine | width = pixelSize }
                |> List.singleton
                |> Collage.collage width height
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


subscriptions : model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (Time.millisecond * 100) Tick
        , Keyboard.downs KeyPress
        ]
