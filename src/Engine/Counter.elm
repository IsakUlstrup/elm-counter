module Engine.Counter exposing (ButtonState, Counter, addCount, decayCount, isDone, new, setHolding, setIdle, tick, toString)


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


decayCount : Float -> Counter -> Counter
decayCount dt counter =
    case counter.state of
        Idle ->
            if not (isDone counter) then
                { counter | count = counter.count - (dt / 2 |> round) |> max 0 }

            else
                counter

        Holding _ ->
            counter


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
