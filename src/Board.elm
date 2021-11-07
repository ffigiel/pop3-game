module Board exposing
    ( Board
    , Piece(..)
    , chainOfSameColor
    , generator
    , isGameOver
    , minChain
    , piecesQueueGenerator
    , queueSize
    , removePieces
    )

import Array
import Array2d exposing (Array2d)
import Random exposing (Generator)
import Set exposing (Set)


minChain : Int
minChain =
    3


size : { width : Int, height : Int }
size =
    { width = 6, height = 8 }


queueSize : Int
queueSize =
    size.width * size.height


generator : Generator Board
generator =
    pieceGenerator
        |> Random.list size.width
        |> Random.list size.height
        |> Random.map Array2d.fromList


piecesQueueGenerator : Generator (List Piece)
piecesQueueGenerator =
    Random.list queueSize pieceGenerator


pieceGenerator : Generator Piece
pieceGenerator =
    Random.uniform Red pieces


type alias Board =
    Array2d Piece


type Piece
    = Red
    | Yellow
    | Green
    | Blue
    | Purple


pieces : List Piece
pieces =
    [ Red
    , Yellow
    , Green
    , Blue
    , Purple
    ]


type alias BoardSearch =
    Array2d { piece : Piece, visited : Bool }


chainOfSameColor : Piece -> ( Int, Int ) -> Board -> Set ( Int, Int )
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
            chainOfSameColorHelper piece ( x, y ) ( boardSearch, Set.empty )
    in
    result


chainOfSameColorHelper :
    Piece
    -> ( Int, Int )
    -> ( BoardSearch, Set ( Int, Int ) )
    -> ( BoardSearch, Set ( Int, Int ) )
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
        ( newBoardSearch, Set.insert ( x, y ) results )
            |> chainOfSameColorHelper piece ( x + 1, y )
            |> chainOfSameColorHelper piece ( x - 1, y )
            |> chainOfSameColorHelper piece ( x, y + 1 )
            |> chainOfSameColorHelper piece ( x, y - 1 )

    else
        ( newBoardSearch, results )


removePieces : Set ( Int, Int ) -> List Piece -> Board -> ( Board, List Piece )
removePieces removedPieces piecesQueue board =
    let
        ( _, colLength ) =
            Array2d.size board

        withPiecesRemoved : Array2d (Maybe Piece)
        withPiecesRemoved =
            board
                |> Array2d.indexedMap
                    (\x y piece ->
                        if Set.member ( x, y ) removedPieces then
                            Nothing

                        else
                            Just piece
                    )

        withPiecesShifted =
            withPiecesRemoved
                |> Array2d.transpose
                |> Array.map
                    (\col ->
                        let
                            filteredCol =
                                Array.filter (\m -> m /= Nothing) col

                            remainder =
                                Array.repeat (colLength - Array.length filteredCol) Nothing
                        in
                        Array.append remainder filteredCol
                    )
                |> Array2d.transpose

        blankCoords =
            withPiecesShifted
                |> Array2d.indexedMap
                    (\x y p ->
                        if p == Nothing then
                            Just ( x, y )

                        else
                            Nothing
                    )
                |> Array2d.foldl
                    (\el coords ->
                        case el of
                            Just xy ->
                                xy :: coords

                            Nothing ->
                                coords
                    )
                    []

        ( withNewPieces, newQueue ) =
            List.foldl
                (\( x, y ) ( b, q ) ->
                    let
                        newB =
                            Array2d.set x y (List.head q) b

                        newQ =
                            List.drop 1 q
                    in
                    ( newB, newQ )
                )
                ( withPiecesShifted, piecesQueue )
                blankCoords
    in
    ( Array2d.map (Maybe.withDefault Red) withNewPieces, newQueue )


isGameOver : Board -> Bool
isGameOver board =
    board
        |> Array2d.indexedMap (\x y piece -> ( x, y, piece ))
        |> Array2d.toList
        |> List.concat
        |> List.all
            (\( x, y, piece ) ->
                let
                    chain =
                        chainOfSameColor piece ( x, y ) board
                in
                Set.size chain < minChain
            )
