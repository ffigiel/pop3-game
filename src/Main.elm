module Main exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



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
    , debug : String
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
            , debug = ""
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
    = ClickedPiece ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPiece ( x, y ) ->
            let
                debug =
                    "Clicked " ++ String.fromInt x ++ ", " ++ String.fromInt y
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
    in
    H.button
        [ HA.class "gamePiece"
        , HA.class colorClass
        , HE.onClick <| ClickedPiece ( y, x )
        ]
        []
