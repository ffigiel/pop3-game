port module Main exposing (main)

import Array
import Board exposing (Board, Piece(..))
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Process
import Random
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Task



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port saveHighScore : Int -> Cmd msg



-- MODEL


type alias Flags =
    JD.Value


type alias Model =
    { board : Board
    , time : Float
    , piecesQueue : List Piece
    , score : Int
    , highScore : Maybe Int
    , isNewHighScore : Bool
    , removedPieces : RemovedPieces
    , fallingPieces : FallingPieces
    }


type alias RemovedPieces =
    Dict ( Int, Int ) Piece


type alias FallingPieces =
    Dict ( Int, Int ) { start : Float, distance : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { board = Array.empty
            , time = 0
            , piecesQueue = []
            , score = 0
            , highScore = highScore
            , isNewHighScore = False
            , removedPieces = Dict.empty
            , fallingPieces = Dict.empty
            }

        highScore =
            JD.decodeValue (JD.field "highScore" JD.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt

        cmd =
            generateBoardCmd
    in
    ( model, cmd )


generateBoardCmd : Cmd Msg
generateBoardCmd =
    Random.map2 (\board piecesQueue -> { board = board, piecesQueue = piecesQueue })
        Board.generator
        Board.piecesQueueGenerator
        |> Random.generate Init



-- UPDATE


type Msg
    = Init { board : Board, piecesQueue : List Piece }
    | Tick Float
    | ClickedPiece Piece ( Int, Int )
    | GotPiecesQueue (List Piece)
    | RemoveAnimationState Int
    | PlayAgainClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init { board, piecesQueue } ->
            ( { model
                | board = board
                , piecesQueue = model.piecesQueue ++ piecesQueue
              }
            , if Board.isGameOver board then
                generateBoardCmd

              else
                Cmd.none
            )

        Tick d ->
            ( { model | time = model.time + d }, Cmd.none )

        ClickedPiece piece ( x, y ) ->
            let
                chain =
                    Board.chainOfSameColor piece ( x, y ) model.board
            in
            if Dict.size chain < Board.minChain then
                ( model, Cmd.none )

            else
                let
                    ( newBoard, newPiecesQueue, fallingPieces ) =
                        Board.removePieces chain
                            model.piecesQueue
                            model.board

                    newScore =
                        model.score + Dict.size chain
                in
                ( { model
                    | score = newScore
                    , removedPieces = chain
                    , board = newBoard
                    , fallingPieces =
                        fallingPieces
                            |> Dict.map
                                (\_ v ->
                                    { start = model.time
                                    , distance = v
                                    }
                                )
                    , piecesQueue = newPiecesQueue
                  }
                , Cmd.batch
                    [ refillPiecesQueue newPiecesQueue
                    , Task.perform (\_ -> RemoveAnimationState model.score) (Process.sleep 500)
                    ]
                )
                    |> handleGameOver

        GotPiecesQueue queue ->
            ( { model | piecesQueue = model.piecesQueue ++ queue }, Cmd.none )

        RemoveAnimationState score ->
            ( if score == model.score then
                { model
                    | removedPieces = Dict.empty
                    , fallingPieces = Dict.empty
                }

              else
                -- player clicked during the transition and a new animation started playing
                model
            , Cmd.none
            )

        PlayAgainClicked ->
            ( { model
                | score = 0
                , isNewHighScore = False
                , removedPieces = Dict.empty
                , fallingPieces = Dict.empty
              }
            , generateBoardCmd
            )


handleGameOver : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
handleGameOver ( model, cmd ) =
    if Board.isGameOver model.board then
        let
            ( highScore, isNewHighScore ) =
                case model.highScore of
                    Nothing ->
                        ( model.score, True )

                    Just hs ->
                        ( max hs model.score, model.score > hs )
        in
        ( { model
            | highScore = Just highScore
            , isNewHighScore = isNewHighScore
          }
        , Cmd.batch
            [ cmd
            , if isNewHighScore then
                saveHighScore highScore

              else
                Cmd.none
            ]
        )

    else
        ( model, cmd )


refillPiecesQueue : List Piece -> Cmd Msg
refillPiecesQueue queue =
    if List.length queue < Board.queueSize then
        Random.generate GotPiecesQueue Board.piecesQueueGenerator

    else
        Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


view : Model -> Html Msg
view model =
    let
        isGameOver =
            Board.isGameOver model.board
    in
    H.div [ HA.class "gameContainer" ]
        [ H.div [ HA.style "position" "relative" ]
            [ viewBoard model
            , if isGameOver then
                viewGameOver model

              else
                H.text ""
            ]
        , if isGameOver then
            H.text ""

          else
            viewScore model
        ]


viewBoard : Model -> Svg Msg
viewBoard model =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x piece ->
                        viewPiece
                            { now = model.time
                            , x = x
                            , y = y
                            , piece = piece
                            , isRemoving = False
                            , fallingFrom =
                                Dict.get ( x, y ) model.fallingPieces
                            }
                    )

        ( width, height ) =
            Board.boardRenderSize
    in
    S.svg
        [ SA.viewBox
            ([ -Board.padding
             , -Board.padding
             , width + 2 * Board.padding
             , height + 2 * Board.padding
             ]
                |> List.map String.fromFloat
                |> String.join " "
            )
        ]
        [ S.g [] <| viewFallingPieces model
        , S.g []
            (Array.toList model.board
                |> List.indexedMap viewRow
                |> List.concat
            )
        ]


viewFallingPieces : Model -> List (Svg Msg)
viewFallingPieces model =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x _ ->
                        case Dict.get ( x, y ) model.removedPieces of
                            Just p ->
                                viewPiece
                                    { now = model.time
                                    , x = x
                                    , y = y
                                    , piece = p
                                    , isRemoving = True
                                    , fallingFrom = Nothing
                                    }

                            Nothing ->
                                H.text ""
                    )
    in
    Array.toList model.board
        |> List.indexedMap viewRow
        |> List.concat


viewPiece :
    { now : Float
    , x : Int
    , y : Int
    , piece : Piece
    , isRemoving : Bool
    , fallingFrom : Maybe { start : Float, distance : Int }
    }
    -> Html Msg
viewPiece { now, x, y, piece, isRemoving, fallingFrom } =
    let
        ( colorClass, symbol ) =
            case piece of
                Red ->
                    ( "-red", "★" )

                Yellow ->
                    ( "-yellow", "▲" )

                Green ->
                    ( "-green", "●" )

                Blue ->
                    ( "-blue", "◆" )

                Purple ->
                    ( "-purple", "■" )

        ( xPos, yPos ) =
            Board.pieceRenderPosition ( x, y )
                |> (\( xp, yp ) ->
                        case fallingFrom of
                            Just f ->
                                let
                                    totalDistance =
                                        (toFloat f.distance * Board.pieceSize)
                                            + (toFloat (f.distance - 1) * Board.gutter)

                                    animationProgress =
                                        (now - f.start)
                                            |> clamp 0 500
                                            |> (\duration -> duration / 500)
                                            |> (\p -> p * p * pi / 2 |> sin)

                                    yOffset =
                                        totalDistance * (1 - animationProgress)
                                in
                                ( xp, yp - yOffset )

                            Nothing ->
                                ( xp, yp )
                   )
    in
    S.g
        [ SA.transform <|
            "translate("
                ++ String.fromFloat (xPos + (Board.pieceSize / 2))
                ++ " "
                ++ String.fromFloat (yPos + (Board.pieceSize / 2))
                ++ ")"
        ]
        [ S.g
            [ SA.class
                ([ ( "gamePiece", True )
                 , ( colorClass, True )
                 , ( "-removing", isRemoving )
                 ]
                    |> List.filterMap
                        (\( c, b ) ->
                            if b then
                                Just c

                            else
                                Nothing
                        )
                    |> String.join " "
                )
            , SE.onClick <| ClickedPiece piece ( x, y )
            ]
            [ S.circle
                [ SA.r <| String.fromFloat <| Board.pieceSize / 2
                ]
                []
            , S.text_
                [ SA.textAnchor "middle"
                , SA.dy "0.7"
                , SA.fontSize "2"
                ]
                [ S.text symbol ]
            ]
        ]


viewGameOver : { a | score : Int, isNewHighScore : Bool } -> Html Msg
viewGameOver { score, isNewHighScore } =
    H.div [ HA.class "gameOverScreen" ]
        [ H.div [ HA.class "gameOverScreen_text" ]
            [ H.p [ HA.class "gameOverScreen_title" ] [ H.text "Game over" ]
            , H.p []
                [ if isNewHighScore then
                    H.text <| "New high score! " ++ String.fromInt score

                  else
                    H.text <| "Score: " ++ String.fromInt score
                ]
            , H.p []
                [ H.button
                    [ HA.type_ "button"
                    , HE.onClick PlayAgainClicked
                    ]
                    [ H.text "Play again?" ]
                ]
            ]
        ]


viewScore : { a | score : Int, highScore : Maybe Int } -> Html Msg
viewScore { score, highScore } =
    H.p [ HA.class "gameScore" ]
        [ H.text <| "Score: " ++ String.fromInt score
        , H.br [] []
        , case highScore of
            Just hs ->
                H.text <| "High score: " ++ String.fromInt (max hs score)

            Nothing ->
                H.text ""
        ]
