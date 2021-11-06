module Board exposing (Board, Piece(..), chainOfSameColor)

import Array2d exposing (Array2d)


type alias Board =
    Array2d Piece


type Piece
    = Red
    | Green
    | Blue


type alias BoardSearch =
    Array2d { piece : Piece, visited : Bool }


chainOfSameColor : Piece -> ( Int, Int ) -> Board -> List ( Int, Int )
chainOfSameColor piece ( x, y ) board =
    let
        boardSearch : BoardSearch
        boardSearch =
            board
                |> Array2d.map
                    (\p ->
                        { piece = p
                        , visited = False
                        }
                    )

        ( _, result ) =
            chainOfSameColorHelper piece ( x, y ) ( boardSearch, [] )
    in
    result


chainOfSameColorHelper :
    Piece
    -> ( Int, Int )
    -> ( BoardSearch, List ( Int, Int ) )
    -> ( BoardSearch, List ( Int, Int ) )
chainOfSameColorHelper piece ( x, y ) ( boardSearch, results ) =
    let
        found =
            boardSearch
                |> Array2d.get x y
                |> Maybe.andThen (\t -> Just <| not t.visited && t.piece == piece)
                |> Maybe.withDefault False

        newBoardSearch =
            boardSearch
                |> Array2d.indexedMap
                    (\tx ty t ->
                        if ty == y && tx == x then
                            { t | visited = True }

                        else
                            t
                    )
    in
    if found then
        ( newBoardSearch, ( x, y ) :: results )
            |> chainOfSameColorHelper piece ( x + 1, y )
            |> chainOfSameColorHelper piece ( x - 1, y )
            |> chainOfSameColorHelper piece ( x, y + 1 )
            |> chainOfSameColorHelper piece ( x, y - 1 )

    else
        ( newBoardSearch, results )
