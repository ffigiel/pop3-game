port module Main exposing (main)

import Array
import Board exposing (Board, Piece(..))
import Browser
import Browser.Events
import Dict exposing (Dict)
import Ease
import Html as H exposing (Attribute, Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Lazy as HL
import I18n exposing (I18n)
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
    { t : I18n
    , board : Board
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
    Dict ( Int, Int ) { start : Float, duration : Float, distance : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            { t = translations
            , board = Array.empty
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
            flags
                |> JD.decodeValue (JD.field "highScore" JD.string)
                |> Result.toMaybe
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        translations =
            flags
                |> JD.decodeValue (JD.field "language" languageDecoder)
                |> Result.withDefault I18n.En
                |> I18n.init

        cmd =
            generateBoardCmd
    in
    ( model, cmd )


languageDecoder : JD.Decoder I18n.Language
languageDecoder =
    JD.string
        |> JD.map
            (\s ->
                I18n.languageFromString s
                    |> Maybe.withDefault I18n.En
            )


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
                        Dict.map
                            (\_ p ->
                                { start = model.time, piece = p }
                            )
                            chain

                    maxDistance =
                        fallingPieces
                            |> Dict.foldl (\_ distance acc -> max distance acc) 0

                    newFallingPieces =
                        fallingPieces
                            |> Dict.map
                                (\_ distance ->
                                    { start = model.time
                                    , duration = calcFallingAnimationDuration distance
                                    , distance = distance
                                    }
                                )

                    animationsDuration =
                        calcFallingAnimationDuration maxDistance
                in
                ( { model
                    | score = newScore
                    , removedPieces = removedPieces
                    , board = newBoard
                    , fallingPieces = newFallingPieces
                    , piecesQueue = newPiecesQueue
                  }
                , Cmd.batch
                    [ refillPiecesQueue newPiecesQueue
                    , Task.perform
                        (\_ -> RemoveAnimationState newScore)
                        (Process.sleep animationsDuration)
                    , if Board.isGameOver newBoard then
                        -- show game over screen with a small delay after click
                        Task.perform
                            (\_ -> GameOver)
                            (Process.sleep gameOverScreenDelay)

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


calcFallingAnimationDuration : Int -> Float
calcFallingAnimationDuration distance =
    -- slightly longer falling animation for higher distances
    logBase (toFloat Board.minChain) (toFloat <| Board.minChain + distance - 1)
        * fallingAnimationBaseDuration


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- VIEW


animationScale : number
animationScale =
    -- for testing
    1


removingAnimationDuration : number
removingAnimationDuration =
    400 * animationScale


fallingAnimationBaseDuration : number
fallingAnimationBaseDuration =
    400 * animationScale


gameOverScreenDelay : number
gameOverScreenDelay =
    200 * animationScale


calcAnimationProgress : Float -> Float -> Float -> Float
calcAnimationProgress tNow tStart duration =
    ((tNow - tStart) / duration)
        |> clamp 0 1


fallingAnimationEasing : Int -> Ease.Easing
fallingAnimationEasing distance =
    Ease.inOut Ease.inBack (easeOutBackDampened distance)


easeOutBackDampened : Int -> Ease.Easing
easeOutBackDampened dampening t =
    -- dampening prevents the pieces from overlapping
    let
        y =
            Ease.outBack t

        fd =
            toFloat dampening
    in
    if y > 1 then
        (y + fd - 1) / fd

    else
        y


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

        gameViewMod =
            if model.isGameOver then
                "-gameOver"

            else
                ""
    in
    H.div [ HA.class "gameContainer" ]
        [ S.svg
            [ SA.id "gameView"
            , SA.class gameViewMod
            , SA.viewBox
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
                [ HL.lazy3 viewScore model.t model.score model.highScore
                ]
            ]
        , if model.isGameOver then
            HL.lazy3 viewGameOver model.t model.score model.isNewHighScore

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
    | PieceFalling { start : Float, duration : Float, distance : Int }


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
                                        calcAnimationProgress now f.start removingAnimationDuration
                                            |> Ease.outQuad

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
                                        calcAnimationProgress now f.start f.duration
                                            |> fallingAnimationEasing f.distance

                                    yOffset =
                                        totalDistance * (1 - animationProgress)
                                in
                                ( xp, yp - yOffset, [] )
                   )

        clickDecoder =
            JD.succeed <| ClickedPiece piece ( x, y )
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
             , SE.on "mousedown" clickDecoder
             , SE.on "touchstart" clickDecoder
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


viewGameOver : I18n -> Int -> Bool -> Html Msg
viewGameOver t score isNewHighScore =
    H.div [ HA.class "gameOverScreen" ]
        [ H.div [ HA.class "gameOverScreen_text" ]
            [ H.p
                [ HA.class "gameOverScreen_title"
                ]
                [ H.text t.gameOver_
                ]
            , H.div []
                [ H.p []
                    [ if isNewHighScore then
                        H.text <| t.newHighScore_ <| String.fromInt score

                      else
                        H.text <| t.gameScore_ <| String.fromInt score
                    ]
                , H.p []
                    [ H.button
                        [ HA.type_ "button"
                        , HE.onClick PlayAgainClicked
                        ]
                        [ H.text t.playAgainBtn_ ]
                    ]
                ]
            , if score > Board.myHighScore then
                H.div
                    [ HA.class "gameOverScreen_congrats"
                    ]
                    [ H.p []
                        [ H.text t.congratulations_
                        , H.br [] []
                        , H.text t.beatenHighScore_
                        , H.br [] []
                        , H.text t.thanksForPlaying_
                        ]
                    , H.p []
                        [ externalLink
                            [ HA.href "https://github.com/megapctr/pop3-game"
                            , HA.style "font-size" "1.2rem"
                            ]
                            [ H.text t.viewSource_ ]
                        ]
                    ]

              else
                H.text ""
            ]
        ]


viewScore : I18n -> Int -> Int -> Html Msg
viewScore t score highScore =
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
            (t.gameScore_ <| String.fromInt score)
        , text
            [ SA.y <| String.fromFloat (gap + textHeight)
            ]
            (t.gameHighScore_ <| String.fromInt (max highScore score))
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
