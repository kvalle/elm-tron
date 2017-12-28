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
    }


width : number
width =
    300


height : number
height =
    200


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
    ( { path = [ ( 0, 0 ), ( 0, 10 ) ]
      , direction = Up
      }
    , Cmd.none
    )


move : Direction -> Path -> Path
move direction path =
    let
        ( x, y ) =
            List.head path |> Maybe.withDefault ( 0, 0 )
    in
        case direction of
            Up ->
                ( x, y + 10 ) :: path

            Down ->
                ( x, y - 10 ) :: path

            Left ->
                ( x - 10, y ) :: path

            Right ->
                ( x + 10, y ) :: path


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
            ( { model | path = move model.direction model.path }
            , Cmd.none
            )

        KeyPress keyCode ->
            ( changeDirection keyCode model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tron" ]
        , div [ class "board" ]
            [ Collage.path model.path
                |> Collage.traced
                    { defaultLine | width = 10 }
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
