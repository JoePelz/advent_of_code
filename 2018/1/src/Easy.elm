module Easy exposing (..)


import Browser
import Html exposing (Html, button, div, h1, h2, hr, p, ul, li, text, textarea)
import Html.Attributes exposing (style, placeholder, value, rows, cols)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { frequency : Int
  , input : String
  , debug : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 "" ""
  , Cmd.none
  )



-- UPDATE


type Msg
  = Change String
  | ResetAll


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change text ->
      ( compute_stream model text
      , Cmd.none
      )
    ResetAll ->
      ( Model 0 "" ""
      , Cmd.none
      )


compute_stream : Model -> String -> Model
compute_stream model text =
  let
    lines = String.lines text
    numbers = List.filterMap String.toInt lines
    number_sum = List.foldl (+) 0 numbers
  in
    { model
    | input = text
    , frequency = number_sum
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = puzzle_title
  , body =
    [ frame [text puzzle_title]
      [ puzzle_description
        , hr [] []
        , form model
        , hr [] []
        , div [] [ text ("Resulting frequency: " ++ String.fromInt model.frequency) ]
        , div
          [ style "color" "#888" ]
          [ text (if String.isEmpty model.debug then "" else ("Debug: " ++ model.debug)) ]
      ]
    ]
  }


form : Model -> Html Msg
form model =
  div []
    [ h2 [] [text "Input data"]
    , textarea
      [ placeholder "-1"
      , value model.input
      , onInput Change
      , rows 5
      , cols 50
      , style "display" "block"
      , style "background-color" "#FFFAF0"
      , style "color" "#234"
      ] []
    , styled_button [ onClick ResetAll] [ text "Clear" ]
    ]


styled_button : List (Html.Attribute msg) -> List (Html msg) -> Html msg
styled_button attrs inner_html =
    button
      ([ style "border-radius" "0.666em"
      , style "background-color" "#FFF"
      , style "padding" "0.3em 1em"
      ] ++ attrs)
      inner_html


frame : List (Html msg) -> List (Html msg) -> Html msg
frame title contents =
  div
    [ style "border" "3px solid #6688AA"
    , style "background-color" "#BBDDFF"
    , style "border-radius" "20px"
    , style "width" "75%"
    , style "max-width" "1024px"
    , style "min-width" "480px"
    , style "min-height" "500px"
    , style "margin" "2em auto"
    , style "padding" "1em 2em"
    , style "position" "relative"
    ]
    ((h1 [] title) :: contents)

puzzle_title : String
puzzle_title = "Day 1: Chronal Calibration"

puzzle_description : Html msg
puzzle_description =
  div []
    [ p [] [text "After feeling like you've been falling for a few minutes, you look at the device's tiny screen. \"Error: Device must be calibrated before first use. Frequency drift detected. Cannot maintain destination lock.\" Below the message, the device shows a sequence of changes in frequency (your puzzle input). A value like +6 means the current frequency increases by 6; a value like -3 means the current frequency decreases by 3."]
    , p [] [text "For example, if the device displays frequency changes of +1, -2, +3, +1, then starting from a frequency of zero, the following changes would occur:"]
    , ul []
      [ li [] [text "Current frequency  0, change of +1; resulting frequency  1."]
      , li [] [text "Current frequency  1, change of -2; resulting frequency -1."]
      , li [] [text "Current frequency -1, change of +3; resulting frequency  2."]
      , li [] [text "Current frequency  2, change of +1; resulting frequency  3."]
      ]
    , p [] [text "In this example, the resulting frequency is 3"]
    , p [] [text "Here are other example situations:"]
    , ul []
      [ li [] [text "+1, +1, +1 results in  3"]
      , li [] [text "+1, +1, -2 results in  0"]
      , li [] [text "-1, -2, -3 results in -6"]
      ]
    , p [] [text "Starting with a frequency of zero, what is the resulting frequency after all of the changes in frequency have been applied?"]
    ]