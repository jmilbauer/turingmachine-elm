module TuringMachineUI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import TuringMachine as TM
import Time exposing (Time, second)
import Color

import Collage as C exposing(defaultLine)
import Element as E
import Text as T

type Msg = Tick | StartMachine | StopMachine
         | SubmitCode
         | LBuff String
         | MBuff String
         | RBuff String
         | SBuff String
         | CBuff String

type alias Model =
    { super : TM.Model
    , leftTapeBuffer : String
    , middleBitBuffer : String
    , rightTapeBuffer : String
    , startStateBuffer : String
    , codeBuffer : String
    , running : Bool
    , sysMsg : String
    }

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

mkModel : TM.Model -> String -> String -> String -> String -> String -> Bool -> String -> Model
mkModel m s1 s2 s3 s4 s5 b sysm =
    { super = m
    , leftTapeBuffer = s1
    , middleBitBuffer = s2
    , rightTapeBuffer = s3
    , startStateBuffer = s4
    , codeBuffer = s5
    , running = b
    , sysMsg = sysm
    }

initialModel : Model
initialModel = mkModel TM.initialModel "" "" "" "" "" False welcome

init = (initialModel, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Tick            -> if model.super.halted
        then ({ model | running = False}, Cmd.none)
        else ({ model | super = TM.runModel model.super }, Cmd.none)
    LBuff str      -> ({ model | leftTapeBuffer = str }, Cmd.none)
    MBuff str      -> ({ model | middleBitBuffer = str }, Cmd.none)
    RBuff str      -> ({ model | rightTapeBuffer = str }, Cmd.none)
    SBuff str      -> ({ model | startStateBuffer = str }, Cmd.none)
    CBuff str      -> ({ model | codeBuffer = str }, Cmd.none)
    SubmitCode     -> case TM.buildModel model.leftTapeBuffer model.middleBitBuffer model.rightTapeBuffer model.startStateBuffer model.codeBuffer of
        Err err     -> ({ model | sysMsg = err, running = False}, Cmd.none)
        Ok smodel   -> ({ model | super = smodel, sysMsg = goodtogo, running = False}, Cmd.none)
    StartMachine    -> ({ model | running = True}, Cmd.none)
    StopMachine     -> ({ model | running = False}, Cmd.none)

view : Model -> Html Msg
view model = div []
    [ layout model ]

instructions : Html Msg
instructions = Html.div []
    [ Html.div [] [Html.text "1. Write in the initial turing machine tape."]
    , Html.div [] [Html.text "2. Choose a starting state for your machine"]
    , Html.div [] [Html.text "3. Write code for your machine"]
    , Html.div [] [Html.text "Code has the format : \"A B C D E\""]
    , Html.div [] [Html.text "A matches the current state"]
    , Html.div [] [Html.text "B matches the symbol at the head"]
    , Html.div [] [Html.text "C is what the symbol is changed to"]
    , Html.div [] [Html.text "D is the direction the tape moves"]
    , Html.div [] [Html.text "E is the new state for the machine"]
    , Html.div [] [Html.text "Then click play!"]
    , Html.div [] [Html.text "The site will try to help you find errors"]
    ]

graphicalDisplay model = Html.span [] [drawModel model.super]

inputFields : Html Msg
inputFields = Html.span []
    [ input [ type_ "text", placeholder "Left Side Tape", onInput LBuff, style [("font-family", "monospace"), ("text-align", "center")] ] []
    , input [ type_ "text", placeholder "Head", onInput MBuff, style [("font-family", "monospace"), ("text-align", "center")], style [("width", "35px")]] []
    , input [ type_ "text", placeholder "Right Side Tape", onInput RBuff, style [("font-family", "monospace"),("text-align", "center")] ] []
    , Html.div [] [input [ type_ "text", placeholder "Start State", onInput SBuff,style [("font-family", "monospace"),("text-align", "center")],  style [("width", "100px")]] []]
    , Html.div [] [textarea [ cols 40, rows 10, placeholder codeBody, onInput CBuff, style [("font-family", "monospace")]] []]
    , button [ onClick SubmitCode ] [ text "Submit"]
    , button [ onClick StartMachine ] [ text "Play" ]
    , button [ onClick StopMachine ] [ text "Pause" ]
    , button [ onClick Tick ] [ text "Step" ]
    ]

codeBody : String
codeBody = "#This code moves the head between two pillars, flipping bits as it goes\nflipright 0 1 > flipright\nflipright 1 0 > flipright\nflipright 2 2 < flipleft\nflipleft 0 1 < flipleft\nflipleft 1 0 < flipleft\nflipleft 2 2 > flipright"

subscriptions : Model -> Sub Msg
subscriptions model = if model.running
    then Sub.batch [ Time.every (Time.millisecond * 100) (always Tick)]
    else Sub.batch []


tablestyle = style
    [ ("border", "5px solid black")
    , ("border-collapse", "collapse")
    ]

cellstyle = style
    [ ("padding", "5px")
    , ("text-align", "center")
    ]

welcome = "Welcome to the Simulator. Read the instructions above."
goodtogo = "Good to go."

bloodHex : String
bloodHex = "#CF232B"

gogogreen : String
gogogreen = "#46A82D"

badtext = style
    [ ("color", bloodHex) ]

goodtext = style
    [ ("color", gogogreen) ]

layout model = Html.table [tablestyle]
    [ Html.tr [] [Html.td [cellstyle, tablestyle, style [("text-align", "left")]] [instructions], Html.td [cellstyle, tablestyle] [inputFields]]
    , Html.tr [] [Html.td [colspan 2, cellstyle, tablestyle, if model.sysMsg == goodtogo || model.sysMsg == welcome then goodtext else badtext] [Html.text model.sysMsg]]
    , Html.tr [] [Html.td [colspan 2, cellstyle, tablestyle] [graphicalDisplay model]]
    ]
--

myline = { defaultLine | width = 2}


zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
    ([],_)              -> []
    (_,[])              -> []
    (x::xs_, y::ys_)    -> (x,y)::(zip xs_ ys_)

toBox : Int -> TM.Symbol -> C.Form
toBox index sym = let
        string = TM.symbolToString sym
        displayText = C.move (0, 2) <| C.text <| T.color Color.black <| T.fromString string
        box = C.filled Color.yellow (C.square 20)
        indexText = C.move (0, -20) <| C.text <| T.color Color.grey <| T.fromString (toString index)
    in
    C.group [indexText, box, displayText]

toBox_ : Int -> TM.Symbol -> C.Form
toBox_ index sym = let
        string = TM.symbolToString sym
        displayText = C.move (0, 2) <| C.text <| T.color Color.black <| T.fromString string
        box = C.filled Color.green (C.square 20)
        indexText = C.move (0, -20) <| C.text <| T.color Color.black <| T.fromString (toString index)
    in
    C.group [indexText, box, displayText]

drawModel : TM.Model -> Html Msg
drawModel tmodel =
    let
        current_mid = tmodel.machine.index
        tapeWidth = 20
        totalTapeLength = 1 + (List.length leftTape) + (List.length rightTape)
        (leftTape, bit, rightTape) = tmodel.machine.tape
        (lSyms, midSym, rSyms) = (List.take tapeWidth (leftTape), bit, List.take tapeWidth (rightTape))
        turingHead = C.filled Color.orange (C.polygon [(0, 0), (40, 20), (40, 50), (-40,50), (-40, 20)])
        headText = C.move (0, 30) <| C.text <| T.color Color.white <| case (TM.getFrom_ tmodel.stateMap tmodel.machine.state) of
            Nothing     -> T.fromString ""
            Just str    -> T.fromString (str)

        leftSymbols = List.map (\(i, s) -> (i, toBox (i - (List.length lSyms) + current_mid) s)) (zip (List.range 0 100) (List.reverse lSyms))
        rightSymbols = List.map (\(i, s) -> (i, toBox (i + current_mid + 1) s)) (zip (List.range 0 100) (rSyms))
        middleSymbol = toBox_ current_mid midSym
        spacedLeft = let
                l = List.length leftSymbols
            in
            List.map (\(p,s) -> C.move (toFloat <| (0) + (-30 * l) + (p * 30), toFloat (0)) s) leftSymbols
        spacedRight = let
                l = List.length rightSymbols
            in
            List.map (\(p,s) -> C.move (toFloat <| (0) + ((p+1) * 30), toFloat (0)) s) rightSymbols
        spacedMiddle = C.move (0,0) middleSymbol
        symbolBoxes = spacedLeft ++ spacedMiddle::spacedRight

        drawnTape = C.move (0, -15) <| C.group (symbolBoxes)
        viewerGroup = C.group [drawnTape, turingHead, headText]

        w = 1000
    in
    E.toHtml (C.collage w 100 ( [C.move (0,0) viewerGroup]))
