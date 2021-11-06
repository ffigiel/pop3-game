module Main exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA



-- UTILS


type alias Array2d a =
    Array (Array a)



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
    { board : Array2d Piece
    }


type Piece
    = Red
    | Green
    | Blue


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { board = board
            }

        board =
            [ [ Red, Green, Green ]
            , [ Red, Green, Blue ]
            , [ Blue, Green, Blue ]
            ]
                |> List.map Array.fromList
                |> Array.fromList

        cmd =
            Cmd.none
    in
    ( model, cmd )



-- UPDATE


type Msg
    = None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    let
        viewRow : Array Piece -> Html Msg
        viewRow row =
            H.div []
                (Array.toList row
                    |> List.map viewPiece
                )
    in
    H.div [ HA.class "gameBoard" ]
        (Array.toList model.board
            |> List.map viewRow
        )


viewPiece : Piece -> Html Msg
viewPiece piece =
    case piece of
        Red ->
            H.div [ HA.class "gamePiece -red" ] []

        Green ->
            H.div [ HA.class "gamePiece -green" ] []

        Blue ->
            H.div [ HA.class "gamePiece -blue" ] []
