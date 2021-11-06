module Main exposing (main)

import Browser
import Html as H exposing (Html)



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
    {}


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            {}

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
view _ =
    H.text ""
