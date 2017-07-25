module TuringMachine exposing (..)

import Array as A exposing (Array)
import String as S
import Result as R

type alias Zipper a = (List a, a, List a)
type Dir = Left | Right | Stay

type Symbol = Symbol Int
type State = State Int
halt = State (-1)
type alias Tape = Zipper Symbol
type Program = Program (List ((State, Symbol), (Dir, Symbol, State)))

type alias TuringMachine =
    { tape : Tape
    , state : State
    , index : Int
    }

type alias Model =
    { machine : TuringMachine
    , program : Program
    , haltState : State
    , stateMap : List (String, State)
    , halted : Bool
    }

initialModel : Model
initialModel = case buildModel "0" "0" "0" "halt" "" of
    Err _   -> Debug.crash "error"
    Ok x    -> x

iterate : Int -> (a -> a) -> a -> a
iterate i f x = if i <= 0 then x else iterate (i-1) f (f x)


------------------------------------------------------------------------
-- Interpreting User Code
------------------------------------------------------------------------

buildStates : String -> (List (String, State), List State, List State)
--                                             ^^check     ^^goto
buildStates str =
    let
        lines = List.filter (\s -> not (comment s)) (S.lines str)
        frst (a,b,c) = a
        scnd (a,b,c) = b
        thrd (a,b,c) = c
        foo acc ls = case (ls) of
            []      -> acc
            l::ls_   -> case (S.words l) of
                (a::b::c::d::e::[])  -> let accc = frst acc in case accc of
                    ((_, State x)::_)  -> case (getFrom accc a, getFrom accc e) of
                        (Nothing, Nothing)  -> if a /= (e)
                            then foo
                                    (((a, State (x+2))::(e, State (x+1))::accc), (State (x+2))::(scnd acc), (State (x+1))::(thrd acc))
                                    ls_
                            else foo
                                    (((a,State (x+1))::accc), State (x+1)::(scnd acc), State(x+1)::(thrd acc))
                                    ls_
                        (Nothing, Just ee)  -> case (List.member ee (thrd acc)) of
                            (False)         -> foo (((a, State (x+1))::accc), (State (x+1))::(scnd acc), (ee)::(thrd acc)) ls_
                            _               -> foo (((a, State (x+1))::accc), (State (x+1))::(scnd acc), thrd acc) ls_

                        (Just aa, Nothing)  -> case (List.member aa (scnd acc)) of
                            (False)         -> foo (((e, State (x+1))::accc), (aa)::(scnd acc), State (x+1)::(thrd acc)) ls_
                            _               -> foo (((e, State (x+1))::accc), scnd acc, State (x+1)::(thrd acc)) ls_

                        (Just aa, Just ee)    -> case Debug.log "" (List.member aa (scnd acc), List.member ee (thrd acc)) of
                            (True, True)    -> foo acc ls_
                            (True, False)   -> foo ((\(x,y,z) -> (x,y,ee::z)) acc) ls_
                            (False, True)   -> foo ((\(x,y,z) -> (x,aa::y,z)) acc) ls_
                            (False, False)  -> foo ((\(x,y,z) -> (x, aa::y, ee::z)) acc) ls_

                    []              -> if (Debug.log "compare a b" (a /= e))
                        then foo
                                ((a, State 2)::(e, State 1)::accc, (State 2)::(scnd acc),(State 1)::(thrd acc))
                                ls_
                        else foo
                                (((a,State 1)::accc), (State 1)::(scnd acc), (State 1)::(thrd acc))
                                ls_
                _  -> (foo acc ls_)
    in
    foo ([("halt", halt)],[halt],[]) lines


--returns the first unchcked state
getUnchecked : List (State) -> List (State) -> Maybe State
--checks if every goto state is checked, besides the halter.
getUnchecked checkStates gotoStates =
    case gotoStates of
        []      -> Nothing
        s::ss   -> if s == halt || List.member s checkStates
            then getUnchecked checkStates ss
            else (Just s)

parseCode : String -> R.Result String (Program)
parseCode str =
    let
        (stateMap, checkStates, gotoStates) = buildStates str
        lines = List.filter (\s -> not (comment s)) (S.lines str)
        foo c acc ls =
            case ls of
                []      -> Ok (Program acc)
                l::ls_  -> case parseLine l stateMap of
                    Err estr           -> Err ("On line " ++ toString c ++ ": " ++ estr)
                    Ok (st,s,ns,d,nst) ->
                        let
                            --turingFunction machine = setState nst <| moveHead d <| setSymbol ns machine
                            opPair = ((st, s), (d, ns, nst))
                        in
                            foo (c+1) (opPair::acc) ls_
    in
    case getUnchecked checkStates gotoStates of
        Nothing     -> case foo 1 [] lines of
            Err err             -> Err err
            Ok (Program res)    -> Ok (Program (List.reverse res))
        Just bst    -> case getFrom_ stateMap bst of
            Nothing     -> Err ("Unchecked goto-state " ++ str ++ " never appeared in initial pass.")
            Just str    -> Err ("At least one non-halting states are unchecked, namely: " ++ str)

getFrom : List (String, State) -> String -> Maybe State
getFrom xs x = case xs of
    []          -> Nothing
    (a,b)::as_  -> if a == x then Just b else getFrom as_ x


getFrom_ : List (String, State) -> State -> Maybe String
getFrom_ xs x = case xs of
    []          -> Nothing
    (a,b)::as_  -> if b == x then Just a else getFrom_ as_ x

stringToDir : String -> Maybe Dir
stringToDir str = case str of
    ">"     -> Just Right
    "<"     -> Just Left
    "-"     -> Just Stay
    "|"     -> Just Stay
    _       -> Nothing


goodSymbol : String -> Bool
goodSymbol str = case str of
    "*"     -> True
    "_"     -> True
    _       -> case S.toInt str of
        R.Err _ -> False
        R.Ok x  -> x >= 0

parseLine : String -> List (String, State) -> R.Result String (State, Symbol, Symbol, Dir, State)
parseLine str stateMap =
    case S.words str of
        st::s::ns::d::nst::[]   -> if not (goodSymbol s) then R.Err ("tape symbol not an integer (symbol: " ++ s ++ ")")
            else if not (goodSymbol ns) then R.Err ("new symbol not an integer (symbol: " ++ ns ++ ")")
            else {-let
                s = if temp_s == "*" then "-2" else temp_s
                ns = if temp_ns == "*" then "-2" else temp_ns
            in -}
            case getFrom stateMap st of
                Nothing         -> R.Err ("state not indexed in initial pass. (state: " ++ st ++ ")")
                Just x          -> let startState = x in
            case stringToSymbol s of
                R.Err _           -> R.Err ("tape symbol not valid (symbol: " ++ s ++ ")")
                R.Ok x            -> let matchSymbol = x in
            case stringToSymbol ns of
                R.Err _           -> R.Err ("new symbol not valid (symbol: " ++ ns ++ ")")
                R.Ok x            -> let newSymbol = x in
            case stringToDir d of
                Nothing         -> R.Err ("direction symbol not recognized (symbol: " ++ d ++ ")")
                Just x          -> let moveDir = x in
            case getFrom stateMap nst of
                Nothing         -> R.Err ("new state not indexed in initial pass (state: " ++ nst ++ ")")
                Just x          -> let newState = x in
            R.Ok (startState, matchSymbol, newSymbol, moveDir, newState)
        _                       -> R.Err "incorrect number of arguments."

----------------------------------------------------------------
-- Running the machine
----------------------------------------------------------------

buildFunction : (Dir, Symbol, State) -> TuringMachine -> TuringMachine
buildFunction (d, s, st) machine =
    let
        (leftTape, bit, rightTape) = machine.tape
    in
    moveHead d <| { machine | state = st, tape = (leftTape, s, rightTape) }

run : Program -> (State, Symbol) -> Maybe (TuringMachine -> TuringMachine)
run (Program program) (st, s) = case program of
    []              -> Nothing
    ((a,b),(nd, ns, nst))::fs   -> if a == st && (b == s || b == (Symbol (-2)))
        then if ns == (Symbol -2)
            then Just (buildFunction (nd, s, nst))
            else Just (buildFunction (nd, ns, nst))
        else run (Program fs) (st, s)

initialMachine : TuringMachine
initialMachine =
    { tape = ([Symbol 0, Symbol 0], Symbol 0, [Symbol 0, Symbol 1])
    , state = State -1
    , index = 0
    }

symbolToString : Symbol -> String
symbolToString (Symbol x) = case x of
    (-1)    -> "_"
    (-2)    -> "*"
    _       -> toString x

stateToString : State -> String
stateToString (State x) = toString x

modelToString : Model -> String
modelToString model =
    let
        (leftTape, bit, rightTape) = model.machine.tape
        leftString = List.take 10 ((List.map symbolToString leftTape) ++ (List.repeat 10 "0"))
        rightString = List.take 10 ((List.map symbolToString rightTape) ++ (List.repeat 10 "0"))
        midbit = symbolToString bit
        line1 = "..." ++
                (List.foldr (++) "" (List.reverse leftString)) ++ "||" ++
                midbit ++ "||" ++
                (List.foldr (++) "" rightString) ++
                "..."

        line2 = "             ^             "
        line3 = "            /\\            "
        line4 = case getFrom_ model.stateMap model.machine.state of
            Nothing -> "state: none"
            Just s  -> "state: " ++ s
    in
    line1 ++ "\n" ++ line2 ++ "\n" ++ line3 ++ "\n" ++ line4

iter : Program -> TuringMachine -> TuringMachine
iter program machine =
    let
        (leftTape, (Symbol bit), rightTape) = machine.tape
        (State state) = machine.state
        operation = run program (State state, Symbol bit)
    in
    if (State state) == halt then machine else
    case operation of
        Nothing     -> machine
        Just op     -> op machine

mvLeft = moveHead Left
mvRight = moveHead Right

setSymbol : Symbol -> TuringMachine -> TuringMachine
setSymbol s machine =
    let (leftTape, bit, rightTape) = machine.tape in
    { machine | tape = (leftTape, s, rightTape) }

setState : State -> TuringMachine -> TuringMachine
setState st machine =
    { machine | state = st }

moveHead : Dir -> TuringMachine -> TuringMachine
moveHead d machine =
    let (leftTape, bit, rightTape) = machine.tape in
    if d == Left then
        case (leftTape) of
            x::xs   -> { machine | tape = (xs, x, bit::rightTape), index = machine.index - 1}
            []      -> { machine | tape = ([], Symbol -1, bit::rightTape), index = machine.index - 1}
    else if d == Right then
        case (rightTape) of
            x::xs   -> { machine | tape = (bit::leftTape, x, xs), index = machine.index + 1}
            []      -> { machine | tape = (bit::leftTape, Symbol -1, []), index = machine.index + 1}
    else
        machine

comment : String -> Bool
comment str = case S.left 1 str of
    "#" -> True
    _   -> case S.words str of
        [""]    -> True
        _       -> False

buildSymbols : List Symbol -> List String -> R.Result String (List Symbol)
buildSymbols acc strs = case strs of
    []      -> Ok (List.reverse acc)
    s::ss   -> case stringToSymbol s of
        Err _   -> Err ("Encountered bad symbol while reading initial model. (symbol: " ++ s ++ ")")
        Ok sym  -> buildSymbols (sym::acc) ss


stringToSymbol : String -> R.Result String Symbol
stringToSymbol str = if goodSymbol str
    then case str of
        "*" -> Ok (Symbol -2)
        "_" -> Ok (Symbol -1)
        _   -> case S.toInt str of
            Err _       -> Err "Could not convert string"
            Ok x        -> Ok (Symbol x)
    else Err "Could not convert string"


parseTape : String -> R.Result String (List Symbol)
parseTape str =
    let
        ls = List.filter (\s -> not (comment s)) (S.lines str)
    in
    case ls of
        [l] -> buildSymbols [] (S.words l)
        _   -> Err "Error: intial tape not correctly formatted"


parseBit : String -> R.Result String (Symbol)
parseBit str = case S.words str of
    [s] -> case stringToSymbol s of
        Err msg -> Err "Could not read initial middle symbol"
        Ok x    -> Ok x
    _   -> Err "Initial middle symbol input malformed"

parseState : String -> List (String, State) -> R.Result String State
parseState str stateMap = case getFrom stateMap str of
    Nothing -> Err "Starting state not indexed."
    Just st -> Ok st


mkModel : List (String, State) -> State -> List Symbol -> List Symbol -> Symbol -> Program -> Model
mkModel sm start rt lt m p =   { machine = {tape = (lt, m, rt), state = start, index = 0}
                                , program = p
                                , haltState = halt
                                , stateMap = sm
                                , halted = False
                                }





buildModel : String -> String -> String -> String -> String -> R.Result String Model
buildModel ltText mtText rtText stText codeText =
    let
        (stateMap, checkStates, gotoStates) = buildStates codeText
    in
    parseCode codeText |> R.andThen (\code ->
    parseBit mtText |> R.andThen (\bit ->
    parseTape ltText |> R.andThen (\leftTape ->
    parseTape rtText |> R.andThen (\rightTape ->
    (parseState stText stateMap) |> R.andThen (\startState ->
    R.Ok (   { machine = { tape = (List.reverse leftTape, bit, rightTape), state = startState, index = 0 }
        , program = code
        , haltState = halt
        , stateMap = stateMap
        , halted = False
        }
    ))))))
    --
    -- case parseCode codeText of
    --         Err err -> Err err
    --         Ok x -> let code = x in
    --     case parseBit mtText of
    --         Err err -> Err err
    --         Ok x  -> let bit = x in
    --     case parseTape ltText of
    --         Err err -> Err err
    --         Ok x    -> let leftTape = List.reverse x in
    --     case parseTape rtText of
    --         Err err -> Err err
    --         Ok x    -> let rightTape = x in
    --     case parseState stText stateMap of
    --         Err err -> Err err
    --         Ok x    -> let startState = x in R.Ok  ({ machine = { tape = (leftTape, bit, rightTape), state = startState, index = 0 }
    --                                                 , program = code
    --                                                 , haltState = halt
    --                                                 , stateMap = stateMap
    --                                                 , halted = False
    --                                                 })

runModel : Model -> Model
runModel model =
    let
        machine = model.machine
        program = model.program
        halter = model.haltState

        newMachine = iter program machine
        newState = newMachine.state
    in
    { model | machine = iter program machine, halted = if newState == halter then True else False }

--code = "0 * * < 88\n\n88 * * > 87\n87 * * > 86\n86 * * > 85\n85 * * > 84\n84 * * > 83\n83 * * > 82\n82 * * > 81\n81 * * > 10\n\n10 0 * < 10\n10 1 0 - 891\n10 _ * > halt\n\n901 * * > 891\n891 * * > 881\n881 * * > 871\n871 * * > 861\n861 * * > 851\n851 * * > 841\n841 * * > 831\n831 * * > 821\n821 * * > 811\n811 * * > 71\n\n71 0 1 < 5\n71 1 0 < 71\n71 _ * - 901\n\n5 _ * < 10\n5 * * < 5\n"

unwrap : Result a b -> b
unwrap res = case res of
    Err _   -> Debug.crash "couldn't unwrap"
    Ok x    -> x
