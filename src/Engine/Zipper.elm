module Engine.Zipper exposing (Zipper, length, new, setCurrent, toList)


type Zipper a
    = Zipper
        { prev : List a
        , current : a
        , next : List a
        }


new : a -> List a -> Zipper a
new current next =
    Zipper { prev = [], current = current, next = next }


toList : Zipper a -> List ( Bool, a )
toList (Zipper zipper) =
    let
        falsePairs : List a -> List ( Bool, a )
        falsePairs is =
            List.map (\i -> ( False, i )) is
    in
    falsePairs zipper.prev
        ++ (( True, zipper.current )
                :: falsePairs zipper.next
           )


length : Zipper a -> Int
length (Zipper zipper) =
    List.length zipper.prev + List.length zipper.next + 1


currentIndex : Zipper a -> Int
currentIndex (Zipper zipper) =
    if List.isEmpty zipper.prev then
        0

    else
        List.length zipper.prev + 1


moveLeft : Int -> Zipper a -> Zipper a
moveLeft steps zipper =
    let
        helper : Int -> Zipper a -> Zipper a
        helper s (Zipper z) =
            if s > 0 then
                (case List.reverse z.prev of
                    x :: xs ->
                        Zipper
                            { prev = List.reverse xs
                            , current = x
                            , next = z.current :: z.next
                            }

                    _ ->
                        Zipper z
                )
                    |> helper (s - 1 |> max 0)

            else
                Zipper z
    in
    helper steps zipper


moveRight : Int -> Zipper a -> Zipper a
moveRight steps zipper =
    let
        helper : Int -> Zipper a -> Zipper a
        helper s (Zipper z) =
            if s > 0 then
                (case z.next of
                    x :: xs ->
                        Zipper
                            { prev = z.prev ++ [ z.current ]
                            , current = x
                            , next = xs
                            }

                    _ ->
                        Zipper z
                )
                    |> helper (s - 1 |> max 0)

            else
                Zipper z
    in
    helper steps zipper


setCurrent : Int -> Zipper a -> Zipper a
setCurrent index (Zipper zipper) =
    if index >= 0 && index < length (Zipper zipper) then
        let
            cIndex : Int
            cIndex =
                currentIndex (Zipper zipper)
        in
        if index < cIndex then
            -- move left
            Zipper zipper |> moveLeft (index - cIndex)

        else if index > cIndex then
            -- move right
            Zipper zipper |> moveRight (index - cIndex)

        else
            -- do nothing
            Zipper zipper

    else
        Zipper zipper
