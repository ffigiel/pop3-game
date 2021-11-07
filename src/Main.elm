module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board, Piece(..))
import Browser
import Dict exposing (Dict)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Process
import Random
import Set exposing (Set)
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



-- MODEL


type alias Flags =
    ()


type alias Model =
    { board : Board
    , piecesQueue : List Piece
    , score : Int
    , removedPieces : Set ( Int, Int )
    , fallingPieces : Dict ( Int, Int ) Int
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { board = Array.empty
            , piecesQueue = []
            , score = 0
            , removedPieces = Set.empty
            , fallingPieces = Dict.empty
            }

        initGenerator =
            Random.map2 (\board piecesQueue -> { board = board, piecesQueue = piecesQueue })
                Board.generator
                Board.piecesQueueGenerator

        cmd =
            Cmd.batch
                [ Random.generate Init initGenerator
                ]
    in
    ( model, cmd )



-- UPDATE


type Msg
    = Init { board : Board, piecesQueue : List Piece }
    | ClickedPiece Piece ( Int, Int )
    | UpdateRemovedPieces
    | GotPiecesQueue (List Piece)
    | SkipFallingPieces


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init { board, piecesQueue } ->
            ( { model
                | board = board
                , piecesQueue = model.piecesQueue ++ piecesQueue
              }
            , Cmd.none
            )

        ClickedPiece piece ( x, y ) ->
            let
                chain =
                    Board.chainOfSameColor piece ( x, y ) model.board
            in
            if Set.size chain < Board.minChain then
                ( model, Cmd.none )

            else
                ( { model
                    | score = model.score + Set.size chain
                    , removedPieces = chain
                  }
                , Task.perform (\_ -> UpdateRemovedPieces) (Process.sleep 500)
                )

        UpdateRemovedPieces ->
            let
                ( newBoard, newPiecesQueue, fallingPieces ) =
                    Board.removePieces model.removedPieces
                        model.piecesQueue
                        model.board
            in
            ( { model
                | board = newBoard
                , removedPieces = Set.empty
                , fallingPieces = fallingPieces
                , piecesQueue = newPiecesQueue
              }
            , refillPiecesQueue newPiecesQueue
            )

        GotPiecesQueue queue ->
            ( { model | piecesQueue = model.piecesQueue ++ queue }
            , Task.perform (\_ -> SkipFallingPieces) (Process.sleep 500)
            )

        SkipFallingPieces ->
            ( { model | fallingPieces = Dict.empty }, Cmd.none )


refillPiecesQueue : List Piece -> Cmd Msg
refillPiecesQueue queue =
    if List.length queue < Board.queueSize then
        Random.generate GotPiecesQueue Board.piecesQueueGenerator

    else
        Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                viewGameOver model.score

              else
                H.text ""
            ]
        , if isGameOver then
            H.text ""

          else
            H.p [ HA.class "gameScore" ]
                [ H.text <| "Score: " ++ String.fromInt model.score
                ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        viewRow : Int -> Array Piece -> Html Msg
        viewRow y row =
            H.div []
                (Array.toList row
                    |> List.indexedMap
                        (\x piece ->
                            viewPiece
                                { x = x
                                , y = y
                                , piece = piece
                                , isRemoving = Set.member ( x, y ) model.removedPieces
                                , fallingFrom =
                                    Dict.get ( x, y ) model.fallingPieces
                                        |> Maybe.withDefault 0
                                }
                        )
                )
    in
    H.div [ HA.class "gameBoard" ]
        (Array.toList model.board
            |> List.indexedMap viewRow
        )


viewPiece : { x : Int, y : Int, piece : Piece, isRemoving : Bool, fallingFrom : Int } -> Html Msg
viewPiece { x, y, piece, isRemoving, fallingFrom } =
    let
        colorClass =
            case piece of
                Red ->
                    "-red"

                Yellow ->
                    "-yellow"

                Green ->
                    "-green"

                Blue ->
                    "-blue"

                Purple ->
                    "-purple"
    in
    H.button
        [ HA.class "gamePiece"
        , HA.class colorClass
        , HA.classList
            [ ( "-removing", isRemoving )
            , ( "-falling", fallingFrom > 0 )
            , ( "-fallingFrom" ++ String.fromInt fallingFrom, fallingFrom > 0 )
            ]
        , HE.onClick <| ClickedPiece piece ( x, y )
        ]
        []


viewGameOver : Int -> Html Msg
viewGameOver score =
    H.div [ HA.class "gameOverScreen" ]
        [ H.div [ HA.class "gameOverScreen_text" ]
            [ H.p [ HA.class "gameOverScreen_title" ] [ H.text "Game over" ]
            , H.p [] [ H.text <| "Score: " ++ String.fromInt score ]
            ]
        ]
