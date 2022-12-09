module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Day01
import Day02
import Day03
import Day04
import Day05
import Debug.Extra
import Html as H
import Url


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , property : String
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url "modelInitialValue", Cmd.none )


type Msg
    = Msg1
    | Msg2
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg1 ->
            ( model, Cmd.none )

        Msg2 ->
            ( model, Cmd.none )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Browser.Document Msg
view _ =
    { title = "Advent of Code 2022"
    , body =
        [ H.pre []
            [ H.text "Day 1\n"
            , H.text "-----\n"
            , H.text "Part 1: "
            , H.text (Debug.toString Day01.part1)
            , H.text "\nPart 2: "
            , H.text (Debug.toString Day01.part2)
            , H.text "\n\n"
            , H.text "Day 2\n"
            , H.text "-----\n"
            , H.text "Part 1: "
            , H.text (Debug.toString Day02.part1)
            , H.text "\nPart 2: "
            , H.text (Debug.toString Day02.part2)
            , H.text "\n\n"
            , H.text "Day 3\n"
            , H.text "-----\n"
            , H.text "Part 1: "
            , H.text (Debug.toString Day03.part1)
            , H.text "\nPart 2: "
            , H.text (Debug.toString Day03.part2)
            , H.text "\n\n"
            , H.text "Day 4\n"
            , H.text "-----\n"
            , H.text "Part 1: "
            , H.text (Debug.toString Day04.part1)
            , H.text "\nPart 2: "
            , H.text (Debug.toString Day04.part2)
            , H.text "\n\n"
            , H.text "Day 5\n"
            , H.text "-----\n"
            , H.text "Part 1: "
            , H.text (Debug.toString Day05.part1)
            , H.text "\nPart 2: "
            , H.text (Debug.toString Day05.part2)
            ]
        ]
    }
