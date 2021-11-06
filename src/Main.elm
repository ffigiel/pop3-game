module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board, Piece(..))
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Random



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
    , debug : String
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { board = Array.empty
            , debug = ""
            }

        cmd =
            Random.generate GotBoard Board.generator
    in
    ( model, cmd )



-- UPDATE


type Msg
    = GotBoard Board
    | ClickedPiece Piece ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBoard board ->
            ( { model | board = board }, Cmd.none )

        ClickedPiece piece ( x, y ) ->
            let
                debug =
                    "Clicked "
                        ++ String.fromInt x
                        ++ ", "
                        ++ String.fromInt y
                        ++ "\n"
                        ++ "Found "
                        ++ String.fromInt (List.length results)
                        ++ " matches: "
                        ++ Debug.toString results

                results =
                    Board.chainOfSameColor piece ( x, y ) model.board
            in
            ( { model | debug = debug }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    H.div []
        [ viewBoard model
        , H.pre [] [ H.text model.debug ]
        ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        viewRow : Int -> Array Piece -> Html Msg
        viewRow y row =
            H.div []
                (Array.toList row
                    |> List.indexedMap (viewPiece y)
                )
    in
    H.div [ HA.class "gameBoard" ]
        (Array.toList model.board
            |> List.indexedMap viewRow
        )


viewPiece : Int -> Int -> Piece -> Html Msg
viewPiece y x piece =
    let
        colorClass =
            case piece of
                Red ->
                    "-red"

                Green ->
                    "-green"

                Blue ->
                    "-blue"

                Yellow ->
                    "-yellow"
    in
    H.button
        [ HA.class "gamePiece"
        , HA.class colorClass
        , HE.onClick <| ClickedPiece piece ( x, y )
        ]
        []
