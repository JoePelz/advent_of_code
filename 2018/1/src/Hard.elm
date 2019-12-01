module Hard exposing (..)


import Browser
import Set exposing (Set)
import Html exposing (Html, button, div, h1, h2, hr, p, ul, li, text, textarea)
import Html.Attributes exposing (style, placeholder, value, rows, cols)
import Html.Events exposing (onClick, onInput)
import Debug exposing (..)


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
    first_repeat = find_first_repeat 1000000 (Set.singleton 0) (frequency_offsets text) 0
  in
    { model
    | input = text
    , frequency = case first_repeat of
         Nothing -> 0
         Just freq -> freq
    , debug = case first_repeat of
        Nothing -> "Error finding repeating frequency"
        Just freq -> ""
    }


find_first_repeat : Int -> Set Int -> List Int -> Int -> Maybe Int
find_first_repeat max_iter past_frequencies deltas start_value =
  if max_iter <= 0 then
    Nothing
  else
    case deltas of
      [] -> Nothing
      [x] -> Nothing
      (x::xs) ->
        let
          value = start_value + x
          seen = Set.member value past_frequencies
        in
          case seen of
            True -> Just value
            False ->
              let
                new_deltas = xs ++ [x]
                new_past_frequencies = Set.insert value past_frequencies
              in
                find_first_repeat (max_iter - 1) new_past_frequencies new_deltas value

frequency_offsets : String -> List Int
frequency_offsets text =
  let
    lines = String.lines text
  in
    List.filterMap String.toInt lines


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
puzzle_title = "Day 1: Chronal Calibration - Part 2"

puzzle_description : Html msg
puzzle_description =
  div []
    [ p [] [text "You notice that the device repeats the same frequency change list over and over. To calibrate the device, you need to find the first frequency it reaches twice."]
    , p [] [text "For example, using the same list of changes above, the device would loop as follows:"]
    , ul []
      [ li [] [text "Current frequency  0, change of +1; resulting frequency  1."]
      , li [] [text "Current frequency  1, change of -2; resulting frequency -1."]
      , li [] [text "Current frequency -1, change of +3; resulting frequency  2."]
      , li [] [text "Current frequency  2, change of +1; resulting frequency  3."]
      , li [] [text "(At this point, the device continues from the start of the list.)"]
      , li [] [text "Current frequency  3, change of +1; resulting frequency  4."]
      , li [] [text "Current frequency  4, change of -2; resulting frequency  2, which has already been seen."]
      ]
    , p [] [text "In this example, the first frequency reached twice is 2. Note that your device might need to repeat its list of frequency changes many times before a duplicate frequency is found, and that duplicates might be found while in the middle of processing the list."]
    , p [] [text "Here are other examples:"]
    , ul []
      [ li [] [text "+1, -1 first reaches 0 twice."]
      , li [] [text "+3, +3, +4, -2, -4 first reaches 10 twice."]
      , li [] [text "-6, +3, +8, +5, -6 first reaches 5 twice."]
      , li [] [text "+7, +7, -2, -7, -4 first reaches 14 twice."]
      ]
    , p [] [text "What is the first frequency your device reaches twice?"]
    ]