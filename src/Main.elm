port module Main exposing (main)

import Array
import Board exposing (Board, Piece(..))
import Browser
import Browser.Events
import Dict exposing (Dict)
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
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


port vibrate : () -> Cmd msg



-- MODEL


type alias Flags =
    JD.Value


type alias Model =
    { board : Board
    , time : Float
    , isGameOver : Bool
    , piecesQueue : List Piece
    , score : Int
    , highScore : Int
    , isNewHighScore : Bool
    , removedPieces : RemovedPieces
    , fallingPieces : FallingPieces
    }


type alias RemovedPieces =
    Dict ( Int, Int ) { start : Float, piece : Piece }


type alias FallingPieces =
    Dict ( Int, Int ) { start : Float, distance : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { board = Array.empty
            , time = 0
            , isGameOver = False
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
                |> Maybe.withDefault 0

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
    | GameOver
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
                        model.score + Board.chainScore chain

                    removedPieces =
                        let
                            maxOrder =
                                Dict.foldl
                                    (\_ { order } n ->
                                        max order n
                                    )
                                    0
                                    chain
                        in
                        Dict.map
                            (\_ v ->
                                let
                                    delay =
                                        removingAnimationStagger * toFloat v.order / toFloat (maxOrder + 1)
                                in
                                { start = model.time + delay, piece = v.piece }
                            )
                            chain
                in
                ( { model
                    | score = newScore
                    , removedPieces = removedPieces
                    , board = newBoard
                    , fallingPieces =
                        fallingPieces
                            |> Dict.map
                                (\_ v ->
                                    { start = model.time + fallingAnimationDelay
                                    , distance = v
                                    }
                                )
                    , piecesQueue = newPiecesQueue
                  }
                , Cmd.batch
                    [ refillPiecesQueue newPiecesQueue
                    , Task.perform
                        (\_ -> RemoveAnimationState newScore)
                        (Process.sleep (fallingAnimationDelay + fallingAnimationDuration))
                    , if Board.isGameOver newBoard then
                        -- show game over screen once the animations complete
                        Task.perform
                            (\_ -> GameOver)
                            (Process.sleep (fallingAnimationDelay + fallingAnimationDuration))

                      else
                        Cmd.none
                    , vibrate ()
                    ]
                )

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

        GameOver ->
            let
                highScore =
                    max model.highScore model.score

                isNewHighScore =
                    model.score > model.highScore
            in
            ( { model
                | isGameOver = True
                , highScore = highScore
                , isNewHighScore = isNewHighScore
              }
            , if isNewHighScore then
                saveHighScore highScore

              else
                Cmd.none
            )

        PlayAgainClicked ->
            ( { model
                | isGameOver = False
                , score = 0
                , isNewHighScore = False
                , removedPieces = Dict.empty
                , fallingPieces = Dict.empty
              }
            , generateBoardCmd
            )


refillPiecesQueue : List Piece -> Cmd Msg
refillPiecesQueue queue =
    if List.length queue < Board.queueSize then
        Random.generate GotPiecesQueue Board.piecesQueueGenerator

    else
        Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- VIEW


removingAnimationStagger : number
removingAnimationStagger =
    400


removingAnimationDuration : number
removingAnimationDuration =
    300


fallingAnimationDelay : number
fallingAnimationDelay =
    300


fallingAnimationDuration : number
fallingAnimationDuration =
    400


gap : Float
gap =
    1


boardGutter : Float
boardGutter =
    gap


pieceSize : Float
pieceSize =
    5


textHeight : Float
textHeight =
    2


pieceRenderPosition : ( Int, Int ) -> ( Float, Float )
pieceRenderPosition ( x, y ) =
    let
        pieceSizeWithGutter =
            pieceSize + boardGutter
    in
    ( toFloat x * pieceSizeWithGutter
    , toFloat y * pieceSizeWithGutter
    )


view : Model -> Html Msg
view model =
    let
        ( width, boardHeight ) =
            pieceRenderPosition ( Board.numCols, Board.numRows )
                |> (\( w, h ) -> ( w - boardGutter, h - boardGutter ))

        height =
            boardHeight + gap + textHeight + gap + textHeight
    in
    H.div [ HA.class "gameContainer" ]
        [ S.svg
            [ SA.viewBox
                ([ -gap
                 , -gap
                 , width + 2 * gap
                 , height + 2 * gap
                 ]
                    |> List.map String.fromFloat
                    |> String.join " "
                )
            ]
            [ if Dict.isEmpty model.removedPieces && Dict.isEmpty model.fallingPieces then
                HL.lazy4 viewBoard 0 model.board model.removedPieces model.fallingPieces

              else
                viewBoard model.time model.board model.removedPieces model.fallingPieces
            , S.g
                [ SA.transform <|
                    "translate("
                        ++ String.fromFloat 0
                        ++ " "
                        ++ String.fromFloat (boardHeight + gap)
                        ++ ")"
                ]
                [ HL.lazy2 viewScore model.score model.highScore
                ]
            ]
        , if model.isGameOver then
            HL.lazy2 viewGameOver model.score model.isNewHighScore

          else
            H.text ""
        ]


viewBoard : Float -> Board -> RemovedPieces -> FallingPieces -> Svg Msg
viewBoard time board removedPieces fallingPieces =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x piece ->
                        viewPiece
                            { now = time
                            , x = x
                            , y = y
                            , piece = piece
                            , animation =
                                case Dict.get ( x, y ) fallingPieces of
                                    Just falling ->
                                        PieceFalling falling

                                    Nothing ->
                                        PieceIdle
                            }
                    )
    in
    S.g []
        [ S.g [] <| viewRemovedPieces time board removedPieces
        , S.g []
            (Array.toList board
                |> List.indexedMap viewRow
                |> List.concat
            )
        ]


viewRemovedPieces : Float -> Board -> RemovedPieces -> List (Svg Msg)
viewRemovedPieces time board removedPieces =
    let
        viewRow y row =
            Array.toList row
                |> List.indexedMap
                    (\x _ ->
                        case Dict.get ( x, y ) removedPieces of
                            Just p ->
                                viewPiece
                                    { now = time
                                    , x = x
                                    , y = y
                                    , piece = p.piece
                                    , animation = PieceRemoving { start = p.start }
                                    }

                            Nothing ->
                                H.text ""
                    )
    in
    Array.toList board
        |> List.indexedMap viewRow
        |> List.concat


type PieceAnimation
    = PieceIdle
    | PieceRemoving { start : Float }
    | PieceFalling { start : Float, distance : Int }


viewPiece :
    { now : Float
    , x : Int
    , y : Int
    , piece : Piece
    , animation : PieceAnimation
    }
    -> Html Msg
viewPiece { now, x, y, piece, animation } =
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

        ( xPos, yPos, otherAttrs ) =
            pieceRenderPosition ( x, y )
                |> (\( xp, yp ) ->
                        case animation of
                            PieceIdle ->
                                ( xp, yp, [] )

                            PieceRemoving f ->
                                let
                                    animationProgress =
                                        (now - f.start)
                                            |> clamp 0 removingAnimationDuration
                                            |> (\duration -> duration / removingAnimationDuration)
                                            |> (\p -> p * p)

                                    opacity =
                                        1 - animationProgress

                                    scale =
                                        1 - animationProgress

                                    blur =
                                        0.025 * animationProgress
                                in
                                ( xp
                                , yp
                                , [ SA.opacity <| String.fromFloat opacity
                                  , SA.transform <| "scale(" ++ String.fromFloat scale ++ ")"
                                  , SA.filter <| "blur(" ++ String.fromFloat blur ++ "rem)"
                                  , SA.pointerEvents "none"
                                  ]
                                )

                            PieceFalling f ->
                                let
                                    totalDistance =
                                        toFloat f.distance * (pieceSize + boardGutter)

                                    animationProgress =
                                        (now - f.start)
                                            |> clamp 0 fallingAnimationDuration
                                            |> (\duration -> duration / fallingAnimationDuration)
                                            |> (\p -> p * p * pi / 2 |> sin)

                                    yOffset =
                                        totalDistance * (1 - animationProgress)
                                in
                                ( xp, yp - yOffset, [] )
                   )
    in
    S.g
        [ SA.transform <|
            "translate("
                ++ String.fromFloat (xPos + (pieceSize / 2))
                ++ " "
                ++ String.fromFloat (yPos + (pieceSize / 2))
                ++ ")"
        ]
        [ S.g
            ([ SA.class <| "gamePiece " ++ colorClass
             , SE.onClick <| ClickedPiece piece ( x, y )
             ]
                ++ otherAttrs
            )
            [ S.circle
                [ SA.r <| String.fromFloat <| pieceSize / 2
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


viewGameOver : Int -> Bool -> Html Msg
viewGameOver score isNewHighScore =
    H.div [ HA.class "gameOverScreen" ]
        [ H.div [ HA.class "gameOverScreen_text" ]
            [ H.p [ HA.class "gameOverScreen_title" ] [ H.text "Game over" ]
            , H.div []
                [ H.p []
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
            , if score > Board.myHighScore then
                H.div
                    [ HA.class "gameOverScreen_congrats"
                    ]
                    [ H.p []
                        [ H.text "Congratulations!"
                        , H.br [] []
                        , H.text "You beat my high score."
                        , H.br [] []
                        , H.text "Thanks for playing!"
                        ]
                    , H.p []
                        [ externalLink
                            [ HA.href "https://github.com/megapctr/pop3-game"
                            , HA.style "font-size" "1.2rem"
                            ]
                            [ H.text "View source code ↗" ]
                        ]
                    ]

              else
                H.text ""
            ]
        ]


viewScore : Int -> Int -> Html Msg
viewScore score highScore =
    let
        text attrs content =
            S.text_
                ((SA.fontSize <| String.fromFloat textHeight)
                    :: (SA.dy <| String.fromFloat (textHeight * 0.8))
                    :: attrs
                )
                [ S.text content ]
    in
    S.g []
        [ text []
            ("Score: " ++ String.fromInt score)
        , text
            [ SA.y <| String.fromFloat (gap + textHeight)
            ]
            ("High score: " ++ String.fromInt (max highScore score))
        ]


externalLink : List (Attribute msg) -> List (Html msg) -> Html msg
externalLink otherAttrs children =
    let
        attrs =
            HA.target "_blank"
                :: HA.rel "noopener noreferrer"
                :: otherAttrs
    in
    H.a attrs children
