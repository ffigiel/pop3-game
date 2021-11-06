module Main exposing (main)

import Array exposing (Array)
import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE



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


type alias Board =
    Array2d Piece


type alias Array2d a =
    Array (Array a)


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
    = ClickedPiece Piece ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                    neighborsOfSameColor piece ( x, y ) model.board
            in
            ( { model | debug = debug }, Cmd.none )


type alias BoardSearch =
    Array2d { piece : Piece, visited : Bool }


neighborsOfSameColor : Piece -> ( Int, Int ) -> Board -> List ( Int, Int )
neighborsOfSameColor piece ( x, y ) board =
    let
        boardSearch : BoardSearch
        boardSearch =
            board
                |> Array.map
                    (\r ->
                        r
                            |> Array.map
                                (\p ->
                                    { piece = p
                                    , visited = False
                                    }
                                )
                    )

        ( _, result ) =
            neighborsOfSameColorHelper piece ( x, y ) ( boardSearch, [] )
    in
    result


neighborsOfSameColorHelper :
    Piece
    -> ( Int, Int )
    -> ( BoardSearch, List ( Int, Int ) )
    -> ( BoardSearch, List ( Int, Int ) )
neighborsOfSameColorHelper piece ( x, y ) ( boardSearch, results ) =
    let
        found =
            boardSearch
                |> Array.get y
                |> Maybe.andThen (Array.get x)
                |> Maybe.andThen (\t -> Just <| not t.visited && t.piece == piece)
                |> Maybe.withDefault False

        newBoardSearch =
            boardSearch
                |> Array.indexedMap
                    (\ty row ->
                        row
                            |> Array.indexedMap
                                (\tx t ->
                                    if ty == y && tx == x then
                                        { t | visited = True }

                                    else
                                        t
                                )
                    )
    in
    if found then
        ( newBoardSearch, ( x, y ) :: results )
            |> neighborsOfSameColorHelper piece ( x + 1, y )
            |> neighborsOfSameColorHelper piece ( x - 1, y )
            |> neighborsOfSameColorHelper piece ( x, y + 1 )
            |> neighborsOfSameColorHelper piece ( x, y - 1 )

    else
        ( newBoardSearch, results )


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
        , HE.onClick <| ClickedPiece piece ( x, y )
        ]
        []
