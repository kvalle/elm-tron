module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Collage exposing (defaultLine)
import Element
import Color
import Time exposing (Time)
import Keyboard
import List.Nonempty exposing (Nonempty, (:::))


type alias Model =
    { player1 : Player
    , player2 : Player
    , state : GameState
    }


type alias Player =
    { path : Path
    , direction : Direction
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
    Nonempty Pos


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Pos =
    ( Float, Float )


type Msg
    = NoOp
    | Move
    | Go
    | ChangeDirectionPlayer1 Direction
    | ChangeDirectionPlayer2 Direction
    | NewGame


init : ( Model, Cmd Msg )
init =
    ( { player1 =
            { path = List.Nonempty.fromElement ( -3, 0 )
            , direction = Up
            }
      , player2 =
            { path = List.Nonempty.fromElement ( 3, 0 )
            , direction = Up
            }
      , state = NotStarted
      }
    , Cmd.none
    )


move : Player -> Player
move player =
    let
        ( x, y ) =
            List.Nonempty.head player.path
    in
        { player
            | path =
                case player.direction of
                    Up ->
                        ( x, y + 1 ) ::: player.path

                    Down ->
                        ( x, y - 1 ) ::: player.path

                    Left ->
                        ( x - 1, y ) ::: player.path

                    Right ->
                        ( x + 1, y ) ::: player.path
        }


isOutsideBoard : Pos -> Bool
isOutsideBoard (( x, y ) as pos) =
    (abs x > (width / 2)) || (abs y > (height / 2))


checkPosition : Model -> Model
checkPosition model =
    let
        player1Head =
            List.Nonempty.head model.player1.path

        player1Tail =
            List.Nonempty.tail model.player1.path

        player2Head =
            List.Nonempty.head model.player2.path

        player2Tail =
            List.Nonempty.tail model.player2.path
    in
        { model
            | state =
                if isOutsideBoard player1Head then
                    GameOver
                else if isOutsideBoard player2Head then
                    GameOver
                else if List.member player1Head (player1Tail ++ player2Tail) then
                    GameOver
                else if List.member player2Head (player1Tail ++ player2Tail) then
                    GameOver
                else
                    Running
        }


setDirectionPlayer1 : Direction -> Model -> Model
setDirectionPlayer1 direction model =
    let
        player1 =
            model.player1
    in
        { model | player1 = { player1 | direction = direction } }


setDirectionPlayer2 : Direction -> Model -> Model
setDirectionPlayer2 direction model =
    let
        player2 =
            model.player2
    in
        { model | player2 = { player2 | direction = direction } }


setState : GameState -> Model -> Model
setState gameState model =
    { model | state = gameState }


setPlayer1 : Player -> Model -> Model
setPlayer1 player model =
    { model | player1 = player }


setPlayer2 : Player -> Model -> Model
setPlayer2 player model =
    { model | player2 = player }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Move ->
            ( case model.state of
                Running ->
                    model
                        |> setPlayer1 (move model.player1)
                        |> setPlayer2 (move model.player2)
                        |> checkPosition

                _ ->
                    model
            , Cmd.none
            )

        ChangeDirectionPlayer1 direction ->
            ( model |> setDirectionPlayer1 direction
            , Cmd.none
            )

        ChangeDirectionPlayer2 direction ->
            ( model |> setDirectionPlayer2 direction
            , Cmd.none
            )

        Go ->
            ( model |> setState Running
            , Cmd.none
            )

        NewGame ->
            init


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
                [ Time.every (Time.millisecond * 100) (always Move)
                , Keyboard.downs gameControlKeys
                ]

        NotStarted ->
            Keyboard.downs gameControlKeys

        _ ->
            Sub.none


gameControlKeys : Keyboard.KeyCode -> Msg
gameControlKeys keyCode =
    case keyCode of
        32 ->
            -- Space
            Go

        37 ->
            -- Arrow left
            ChangeDirectionPlayer1 Left

        38 ->
            -- Arrow up
            ChangeDirectionPlayer1 Up

        39 ->
            -- Arrow right
            ChangeDirectionPlayer1 Right

        40 ->
            -- Arrow down
            ChangeDirectionPlayer1 Down

        65 ->
            -- A key
            ChangeDirectionPlayer2 Left

        87 ->
            -- W key
            ChangeDirectionPlayer2 Up

        68 ->
            -- D key
            ChangeDirectionPlayer2 Right

        83 ->
            -- S key
            ChangeDirectionPlayer2 Down

        _ ->
            NoOp


view : Model -> Html Msg
view model =
    let
        pixelSize =
            10
    in
        div [ class "game" ]
            [ div [ class "board" ]
                [ let
                    path player color =
                        if model.state == NotStarted then
                            Collage.square pixelSize
                                |> Collage.filled color
                                |> Collage.move
                                    (List.Nonempty.head player.path
                                        |> Tuple.mapFirst ((*) pixelSize)
                                        |> Tuple.mapSecond ((*) pixelSize)
                                    )
                        else
                            player.path
                                |> List.Nonempty.toList
                                |> List.map (Tuple.mapFirst <| (*) pixelSize)
                                |> List.map (Tuple.mapSecond <| (*) pixelSize)
                                |> Collage.path
                                |> Collage.traced
                                    { defaultLine
                                        | width = pixelSize
                                        , cap = Collage.Padded
                                        , color = color
                                    }

                    players =
                        [ path model.player1 Color.red
                        , path model.player2 Color.blue
                        ]
                  in
                    players
                        |> Collage.collage (width * pixelSize + pixelSize // 2) (height * pixelSize + pixelSize // 2)
                        |> Element.toHtml
                ]
            , div [ class "info" ]
                [ div [ class "title" ] [ text "TRON" ]
                , div [ class "status" ]
                    [ text <|
                        case model.state of
                            NotStarted ->
                                "press SPACE to begin"

                            Running ->
                                ""

                            GameOver ->
                                "game over"
                    ]
                , if model.state == GameOver then
                    button [ class "new-game", onClick NewGame ] [ text "new game" ]
                  else
                    text ""
                ]
            ]
