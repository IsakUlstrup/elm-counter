module Suite exposing (suite)

import Engine.Zipper as Zipper
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Zipper"
        [ describe "Constructors"
            [ test "new singleton" <|
                \_ ->
                    Zipper.new 0 []
                        |> Zipper.toList
                        |> Expect.equalLists [ ( True, 0 ) ]
            , test "new with next" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.toList
                        |> Expect.equalLists [ ( True, 0 ), ( False, 1 ), ( False, 2 ) ]
            , test "length of singleton" <|
                \_ ->
                    Zipper.new 0 []
                        |> Zipper.length
                        |> Expect.equal 1
            , test "length" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.length
                        |> Expect.equal 3
            ]
        , describe "Current"
            [ test "Set current by index" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent 1
                        |> Zipper.toList
                        |> Expect.equalLists [ ( False, 0 ), ( True, 1 ), ( False, 2 ) ]
            , test "Set current by another index" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent 2
                        |> Zipper.toList
                        |> Expect.equalLists [ ( False, 0 ), ( False, 1 ), ( True, 2 ) ]
            , test "Set current by index, then set back" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent 2
                        |> Zipper.setCurrent 0
                        |> Zipper.toList
                        |> Expect.equalLists [ ( True, 0 ), ( False, 1 ), ( False, 2 ) ]
            , test "Set current by index twice" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent 1
                        |> Zipper.setCurrent 1
                        |> Zipper.toList
                        |> Expect.equalLists [ ( False, 0 ), ( True, 1 ), ( False, 2 ) ]
            , test "Set current by out of bounds index" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent 3
                        |> Zipper.toList
                        |> Expect.equalLists [ ( True, 0 ), ( False, 1 ), ( False, 2 ) ]
            , test "Set current by negative out of bounds index" <|
                \_ ->
                    Zipper.new 0 [ 1, 2 ]
                        |> Zipper.setCurrent -1
                        |> Zipper.toList
                        |> Expect.equalLists [ ( True, 0 ), ( False, 1 ), ( False, 2 ) ]
            ]
        ]
