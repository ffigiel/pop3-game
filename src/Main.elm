module Main exposing (main)

import Array exposing (Array)
import Board exposing (Board, Piece(..))
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Random
import Set exposing (Set)



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
    , score : Int
    , removingPieces : Set ( Int, Int )
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { board = Array.empty
            , score = 0
            , removingPieces = Set.empty
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
                matches =
                    Board.chainOfSameColor piece ( x, y ) model.board
            in
            if Set.size matches < 3 then
                ( model, Cmd.none )

            else
                ( { model
                    | score = Set.size matches
                    , removingPieces = matches
                  }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    H.div []
        [ viewBoard model
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
                            viewPiece x y (Set.member ( x, y ) model.removingPieces) piece
                        )
                )
    in
    H.div [ HA.class "gameBoard" ]
        (Array.toList model.board
            |> List.indexedMap viewRow
        )


viewPiece : Int -> Int -> Bool -> Piece -> Html Msg
viewPiece x y isRemoving piece =
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
        , HA.classList [ ( "-removing", isRemoving ) ]
        , HE.onClick <| ClickedPiece piece ( x, y )
        ]
        []
