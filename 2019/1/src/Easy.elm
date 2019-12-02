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
  { fuel_needed : Int
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
    numbers = input_integers text
  in
    { input = text
    , fuel_needed = sum_fuel numbers 0
    , debug = ""
    }


sum_fuel : List Int -> Int -> Int
sum_fuel values initial_value =
  List.foldl (+) initial_value (List.map calculate_fuel values)



calculate_fuel : Int -> Int
calculate_fuel mass =
    (mass // 3) - 2
    
    
input_integers : String -> List Int
input_integers text =
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
        , div [] [ text ("Resulting fuel_needed: " ++ String.fromInt model.fuel_needed) ]
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
puzzle_title = "Day 1: The Tyranny of the Rocket Equation -- Part 1"

puzzle_description : Html msg
puzzle_description =
  div []
    [ p [] [text "The Elves quickly load you into a spacecraft and prepare to launch."]
    , p [] [text "At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper. They haven't determined the amount of fuel required yet."]
    , p [] [text "Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2."]
    , p [] [text "For example:"]
    , ul []
      [ li [] [text "For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2."]
      , li [] [text "For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2."]
      , li [] [text "For a mass of 1969, the fuel required is 654."]
      , li [] [text "For a mass of 100756, the fuel required is 33583."]
      ]
    , p [] [text "The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually calculate the fuel needed for the mass of each module (your puzzle input), then add together all the fuel values."]
    , p [] [text "What is the sum of the fuel requirements for all of the modules on your spacecraft?"]
    ]