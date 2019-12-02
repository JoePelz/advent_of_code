module Hard exposing (..)


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
    let
      fuel = (mass // 3) - 2
    in
      if fuel <= 0 then
        0
      else
        fuel + (calculate_fuel fuel)
    
    
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
    [ p [] ["During the second Go / No Go poll, the Elf in charge of the Rocket Equation Double-Checker stops the "
           ++ "launch sequence. Apparently, you forgot to include additional fuel for the fuel you just added."
           |> text
           ]
    , p [] ["Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract "
           ++ "2. However, that fuel also requires fuel, and that fuel requires fuel, and so on. Any mass that would "
           ++ "require negative fuel should instead be treated as if it requires zero fuel; the remaining mass, if "
           ++ "any, is instead handled by wishing really hard, which has no mass and is outside the scope of this "
           ++ "calculation."
           |> text
           ]
    , p [] ["So, for each module mass, calculate its fuel and add it to the total. Then, treat the fuel amount you "
           ++ "just calculated as the input mass and repeat the process, continuing until a fuel requirement is zero "
           ++ "or negative. For example:"
           |> text
           ]
    , ul []
      [ li [] ["A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded "
              ++ "down is 0, which would call for a negative fuel), so the total fuel required is still just 2."
              |> text
              ]
      , li [] ["At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel "
              ++ "(654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which "
              ++ "requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + "
              ++ "21 + 5 = 966."
              |> text
              ]
      , li [] ["The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 "
              ++ "+ 43 + 12 + 2 = 50346."
              |> text
              ]
      ]
    , p [] ["What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into "
           ++ "account the mass of the added fuel? (Calculate the fuel requirements for each module separately, then "
           ++ "add them all up at the end.)"
           |> text
           ]
    ]