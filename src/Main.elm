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
    , highScore : Maybe Int
    , isNewHighScore : Bool
    , removedPieces : Dict ( Int, Int ) Piece
    , fallingPieces : Dict ( Int, Int ) Int
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { board = Array.empty
            , piecesQueue = []
            , score = 0
            , highScore = Nothing
            , isNewHighScore = False
            , removedPieces = Dict.empty
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
    | GotPiecesQueue (List Piece)
    | RemoveAnimationState Int


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
                    , fallingPieces = fallingPieces
                    , piecesQueue = newPiecesQueue
                  }
                , Cmd.batch
                    [ refillPiecesQueue newPiecesQueue
                    , Task.perform (\_ -> RemoveAnimationState model.score) (Process.sleep 5000)
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
        , cmd
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
            , viewFallingPieces model
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
                                , isRemoving = False
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


viewFallingPieces : Model -> Html Msg
viewFallingPieces model =
    let
        viewRow : Int -> Array Piece -> Html Msg
        viewRow y row =
            H.div []
                (Array.toList row
                    |> List.indexedMap
                        (\x _ ->
                            case Dict.get ( x, y ) model.removedPieces of
                                Just p ->
                                    viewPiece
                                        { x = x
                                        , y = y
                                        , piece = p
                                        , isRemoving = True
                                        , fallingFrom = 0
                                        }

                                Nothing ->
                                    viewPiecePlaceholder
                        )
                )
    in
    H.div [ HA.class "gameBoard -falling" ]
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
        , HA.type_ "button"
        ]
        []


viewPiecePlaceholder : Html Msg
viewPiecePlaceholder =
    H.button [ HA.class "gamePiece -placeholder" ] []


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
