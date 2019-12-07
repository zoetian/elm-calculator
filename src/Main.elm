module Main exposing (Calculator, Model, Msg(..), calculate, calculator, decimal, equal, init, main, operation, parseFloat, update, updateDisplay, zero)

import Html exposing (..)
import Html.Attributes exposing (class)


main =
    { model = init
    , view = view
    , update = update
    }



-- model


type alias Calculator =
    { add : Float -> Float -> Float
    , minus : Float -> Float -> Float
    , times : Float -> Float -> Float
    , divide : Float -> Float -> Float
    }


calculator : Calculator
calculator =
    { add = \x y -> x + y
    , minus = \x y -> x - y
    , times = \x y -> x * y
    , divide = \x y -> x / y
    }


type alias Model =
    { display : String
    , function : Float -> Float -> Float
    , saveValue : Float
    , append : Bool
    }


init : Model
init =
    { display = ""
    , function = \x y -> y
    , saveValue = 0
    , append = True
    }


parseFloat : String -> Float
parseFloat inputString =
    Result.withDefault 0 (String.toFloat inputString)


operation : Model -> (Float -> Float -> Float) -> Model
operation model function =
    { model
        | function = function
        , saveValue = parseFloat model.display
        , append = False
    }



-- view
--button : Msg -> String -> Html Msg
--button msg text =
--    button [ class "key"]
--
--
--view : Model -> Msg
--view =
--
-- update


type Msg
    = None
    | Add
    | Minus
    | Times
    | Divide
    | Equal
    | Decimal
    | Zero
    | Number Int
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        None ->
            model

        Add ->
            operation model calculator.add

        Minus ->
            operation model calculator.minus

        Times ->
            operation model calculator.times

        Divide ->
            operation model calculator.divide

        Equal ->
            equal model

        Decimal ->
            decimal model

        Zero ->
            zero model

        Number number ->
            updateDisplay model number

        Clear ->
            init


updateDisplay : Model -> Int -> Model
updateDisplay model number =
    if model.append then
        { model | display = model.display ++ Debug.toString number }

    else
        { model | display = Debug.toString number, append = True }


equal : Model -> Model
equal model =
    if model.append then
        { model
            | display = calculate modle
            , saveValue = model.display |> parseFloat
            , append = False
        }

    else
        { model
            | display = calculate model
            , append = False
        }


decimal : Model -> Model
decimal model =
    if not (String.isEmpty model.display) && model.append then
        { model | display = appendDecimal model.display }

    else
        { model | display = "0.", append = True }


zero : Model -> Model
zero model =
    if String.isEmpty model.display || not model.append then
        { model
            | display = "0"
            , append = False
        }

    else
        { model
            | display = model.display ++ "0"
        }


calculate : Model -> String
calculate model =
    model.function model.lastValue (parseFloat model.display) |> Debug.toString


appendDecimal : String -> String
appendDecimal string =
    if String.contains "." string then
        string

    else
        string ++ "."

