module Engine.Counter exposing
    ( ButtonState
    , Counter
    , addCount
    , decayCount
    , isDone
    , kill
    , new
    , press
    , release
    , tick
    , toString
    )


type alias Counter =
    { count : Int
    , maxCount : Int
    , state : ButtonState
    , icon : String
    }


type ButtonState
    = Idle
    | Holding Float
    | Dead Float


new : String -> Counter
new icon =
    Counter 0 20 Idle icon


release : Counter -> Counter
release button =
    case button.state of
        Holding _ ->
            { button | state = Idle }

        _ ->
            button


press : Counter -> Counter
press button =
    case button.state of
        Idle ->
            { button | state = Holding 0 }

        _ ->
            button


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

        Dead time ->
            if time == 0 then
                { button | state = Idle }

            else
                { button | state = Dead ((time - dt) |> max 0) }


kill : Counter -> Counter
kill counter =
    if isDone counter then
        { counter
            | state = Dead 5000
            , count = 0
        }

    else
        counter


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

        Dead _ ->
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

        Dead _ ->
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

        Dead _ ->
            "dead"
