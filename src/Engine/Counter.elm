module Engine.Counter exposing (ButtonState, Counter, addCount, isDone, isDoneHolding, new, notEmpty, notFull, setCount, setHolding, setIdle, subtractCount, tick, toString)


type alias Counter =
    { count : Int
    , maxCount : Int
    , state : ButtonState
    , icon : String
    }


type ButtonState
    = Idle
    | Holding Float


new : String -> Counter
new icon =
    Counter 0 20 Idle icon


setCount : Int -> Counter -> Counter
setCount count counter =
    { counter | count = clamp 0 counter.maxCount count }


setIdle : Counter -> Counter
setIdle button =
    { button | state = Idle }


setHolding : Counter -> Counter
setHolding button =
    { button | state = Holding 0 }


tick : Float -> Counter -> Counter
tick dt button =
    case button.state of
        Idle ->
            button

        Holding time ->
            if time == 0 then
                { button | state = Holding 100 }

            else
                { button | state = Holding ((time - dt) |> max 0) }


addCount : Counter -> Counter
addCount button =
    case button.state of
        Idle ->
            button

        Holding time ->
            if time == 0 then
                { button | count = button.count + 1 |> min button.maxCount }

            else
                button


subtractCount : Counter -> Counter
subtractCount button =
    { button | count = button.count - 1 |> max 0 }


isDoneHolding : Counter -> Bool
isDoneHolding button =
    case button.state of
        Holding time ->
            time == 0

        _ ->
            False


isDone : Counter -> Bool
isDone counter =
    counter.count == counter.maxCount


toString : Counter -> String
toString button =
    case button.state of
        Idle ->
            "idle"

        Holding _ ->
            "holding"


notEmpty : Counter -> Bool
notEmpty counter =
    counter.count > 0


notFull : Counter -> Bool
notFull counter =
    counter.count < counter.maxCount
