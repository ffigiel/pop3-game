module Array2d exposing
    ( Array2d
    , empty
    , fromList
    , get
    , indexedMap
    , map
    )

import Array exposing (Array)


type alias Array2d a =
    Array (Array a)


map : (a -> b) -> Array2d a -> Array2d b
map f =
    Array.map (Array.map f)


indexedMap : (Int -> Int -> a -> b) -> Array2d a -> Array2d b
indexedMap f =
    Array.indexedMap (\y -> Array.indexedMap (\x -> f x y))


get : Int -> Int -> Array2d a -> Maybe a
get x y =
    Array.get y
        >> Maybe.andThen (Array.get x)


fromList : List (List a) -> Array2d a
fromList =
    List.map Array.fromList
        >> Array.fromList


empty : Array2d a
empty =
    Array.empty
