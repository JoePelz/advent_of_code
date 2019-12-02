module Easy exposing (..)


import Browser
import Dict exposing (Dict)
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
  { position_0 : Int
  , input : String
  , debug : String
  }

type alias Program = Dict Int Int

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
      ( execute model text
      , Cmd.none
      )
    ResetAll ->
      ( Model 0 "" ""
      , Cmd.none
      )


execute : Model -> String -> Model
execute model code =
  let
    program = code |> input_integers |> indexify
    executed_program = run_instructions program 0
    data_0 = case executed_program of
      Nothing -> Nothing
      Just result -> Dict.get 0 result
  in
    case data_0 of
      Nothing ->
        { input = code
        , position_0 = 0
        , debug = "Error compiling program."
        }
      Just exit_code ->
        { input = code
        , position_0 = exit_code
        , debug = Debug.toString executed_program
        }


run_instructions : Program -> Int -> Maybe (Program)
run_instructions program instruction_number =
  let
    instruction = Dict.get (instruction_number * 4) program
  in
    case instruction of
      Just 1 ->
        let
          result = Debug.log "result" (perform_addition program instruction_number)
        in
          case result of
            Nothing -> Nothing
            Just new_program -> run_instructions new_program (instruction_number + 1)
      Just 2 ->
        let result = (perform_multiplication program instruction_number) in
        case result of
            Nothing -> Nothing
            Just new_program -> run_instructions new_program (instruction_number + 1)
      Just 99 ->
        let
          result = Debug.log "result" program
        in
          Just result
      Just _ -> Nothing
      Nothing -> Nothing


perform_addition : Program -> Int -> Maybe (Program)
perform_addition program operation =
  let
    operand1_register = Debug.log "operand1_register" (Dict.get (operation * 4 + 1) program)
    operand1 = Debug.log "operand1" (case operand1_register of
      Nothing -> Nothing
      Just register -> Dict.get (register) program)
    operand2_register = Debug.log "operand2_register" (Dict.get (operation * 4 + 2) program)
    operand2 = Debug.log "operand2" (case operand2_register of
      Nothing -> Nothing
      Just register -> Dict.get (register) program)
    output_register = Debug.log "output_register" (Dict.get (operation * 4 + 3) program)
  in
    case operand1 of
      Nothing -> Nothing
      Just x -> case operand2 of
        Nothing -> Nothing
        Just y -> case output_register of
          Nothing -> Nothing
          Just output -> Just (Dict.insert output (x + y) program)


perform_multiplication : Program -> Int -> Maybe (Program)
perform_multiplication program operation =
  let
    operand1_register = Debug.log "operand1_register" (Dict.get (operation * 4 + 1) program)
    operand1 = Debug.log "operand1" (case operand1_register of
      Nothing -> Nothing
      Just register -> Dict.get (register) program)
    operand2_register = Debug.log "operand2_register" (Dict.get (operation * 4 + 2) program)
    operand2 = Debug.log "operand2" (case operand2_register of
      Nothing -> Nothing
      Just register -> Dict.get (register) program)
    output_register = Debug.log "output_register" (Dict.get (operation * 4 + 3) program)
  in
    case operand1 of
      Nothing -> Nothing
      Just x -> case operand2 of
        Nothing -> Nothing
        Just y -> case output_register of
          Nothing -> Nothing
          Just output -> Just (Dict.insert output (x * y) program)


indexify : List Int -> Program
indexify instructions =
    Dict.fromList <| List.indexedMap Tuple.pair instructions


input_integers : String -> List Int
input_integers text =
  let
    lines = String.split "," text
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
        , div [] [ text ("Resulting position_0: " ++ String.fromInt model.position_0) ]
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
puzzle_title = "Day 2: 1202 Program Alarm -- Part 1"

puzzle_description : Html msg
puzzle_description =
  div [style "font-family" "Source Code Pro, monospace"]
    [ p [] ["On the way to your gravity assist around the Moon, your ship computer beeps angrily about a \"1202 "
           ++ "program alarm\". On the radio, an Elf is already explaining how to handle the situation: \"Don't worry, "
           ++ "that's perfectly norma--\" The ship computer bursts into flames."
           |> text
           ]
    , p [] [ "You notify the Elves that the computer's magic smoke seems to have escaped. \"That computer ran Intcode "
           ++ "programs like the gravity assist program it was working on; surely there are enough spare parts up "
           ++ "there to build a new Intcode computer!\""
           |> text
           ]
    , p [] [ "An Intcode program is a list of integers separated by commas (like 1,0,0,3,99). To run one, start by "
           ++ "looking at the first integer (called position 0). Here, you will find an opcode - either 1, 2, or 99. "
           ++ "The opcode indicates what to do; for example, 99 means that the program is finished and should "
           ++ "immediately halt. Encountering an unknown opcode means something went wrong."
           |> text
           ]
    , p [] [ "Opcode 1 adds together numbers read from two positions and stores the result in a third position. The "
           ++ "three integers immediately after the opcode tell you these three positions - the first two indicate the "
           ++ "positions from which you should read the input values, and the third indicates the position at which the"
           ++ " output should be stored."
           |> text
           ]
    , p [] [ "For example, if your Intcode computer encounters 1,10,20,30, it should read the values at positions 10 "
           ++ "and 20, add those values, and then overwrite the value at position 30 with their sum."
           |> text
           ]
    , p [] [ "Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, "
           ++ "the three integers after the opcode indicate where the inputs and outputs are, not their values."
           |> text
           ]
    , p [] [ "Once you're done processing an opcode, move to the next one by stepping forward 4 positions."
           |> text
           ]
    , p [] [ "For example, suppose you have the following program:"
           |> text
           ]
    , p [] [ "1,9,10,3,2,3,11,0,99,30,40,50"
           |> text
           ]
    , p [] [ "For the purposes of illustration, here is the same program split into multiple lines:"
           |> text
           ]
    , p [style "white-space" "pre"] [ text """1,9,10,3,
2,3,11,0,
99,
30,40,50"""]
    , p [] [ "The first four integers, 1,9,10,3, are at positions 0, 1, 2, and 3. Together, they represent the first "
           ++ "opcode (1, addition), the positions of the two inputs (9 and 10), and the position of the output (3). "
           ++ "To handle this opcode, you first need to get the values at the input positions: position 9 contains 30, "
           ++ "and position 10 contains 40. Add these numbers together to get 70. Then, store this value at the output "
           ++ "position; here, the output position (3) is at position 3, so it overwrites itself. Afterward, the "
           ++ "program looks like this:"
           |> text
           ]
    , p [style "white-space" "pre"] [ text """1,9,10,70,
2,3,11,0,
99,
30,40,50"""]
    , p [] [ "Step forward 4 positions to reach the next opcode, 2. This opcode works just like the previous, but it "
           ++ "multiplies instead of adding. The inputs are at positions 3 and 11; these positions contain 70 and 50 "
           ++ "respectively. Multiplying these produces 3500; this is stored at position 0:"
           |> text
           ]
    , p [style "white-space" "pre"] [ text """3500,9,10,70,
2,3,11,0,
99,
30,40,50"""]
    , p [] [ "Stepping forward 4 more positions arrives at opcode 99, halting the program."
           |> text
           ]
    , p [] [ "Here are the initial and final states of a few more small programs:"
           |> text
           ]
    , ul []
      [ li [] [text "1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2)."]
      , li [] [text "2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6)."]
      , li [] [text "2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801)."]
      , li [] [text "1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99."]
      ]
    , p [] [ "Once you have a working computer, the first step is to restore the gravity assist program (your puzzle "
           ++ "input) to the \"1202 program alarm\" state it had just before the last computer caught fire. To do "
           ++ "this, before running the program, replace position 1 with the value 12 and replace position 2 with the "
           ++ "value 2. What value is left at position 0 after the program halts?"
           |> text
           ]
    , p [] [text "What is the sum of the fuel requirements for all of the modules on your spacecraft?"]
    ]
