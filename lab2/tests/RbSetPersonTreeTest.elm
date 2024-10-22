module RbSetPersonTreeTest exposing (..)

import Expect
import RbSetTree exposing (..)
import Test exposing (Test, test)


type alias Person =
    { name : String
    , age : Int
    }


personComparator : Comparator Person
personComparator a b =
    if a.age < b.age then
        LT

    else if a.age > b.age then
        GT

    else if a.name < b.name then
        LT

    else if a.name > b.name then
        GT

    else
        EQ


personTree : RBTree Person
personTree =
    insert { name = "Alice", age = 25 } personComparator Empty
        |> insert { name = "Bob", age = 30 } personComparator
        |> insert { name = "Charlie", age = 20 } personComparator
        |> insert { name = "Denise", age = 35 } personComparator
        |> insert { name = "Eve", age = 25 } personComparator
        |> insert { name = "Frank", age = 30 } personComparator
        |> insert { name = "George", age = 20 } personComparator
        |> insert { name = "Helen", age = 35 } personComparator


testInsertEmpty : Test
testInsertEmpty =
    test "Insert into empty tree" <|
        \_ ->
            let
                tree =
                    insert { name = "Alice", age = 25 } personComparator Empty
            in
            case tree of
                Node _ _ value _ ->
                    Expect.equal value { name = "Alice", age = 25 }

                _ ->
                    Expect.fail "Expected a tree with a single node"


testInsertMultiple : Test
testInsertMultiple =
    test "Insert multiple values into person-tree" <|
        \_ ->
            let
                tree =
                    insert { name = "Denise", age = 35 } personComparator personTree
                        |> insert { name = "Ben", age = 30 } personComparator
                        |> insert { name = "Eve", age = 25 } personComparator
            in
            Expect.equal (toList tree) [ { age = 20, name = "Charlie" }, { age = 20, name = "George" }, { age = 25, name = "Alice" }, { age = 25, name = "Eve" }, { age = 30, name = "Ben" }, { age = 30, name = "Bob" }, { age = 30, name = "Frank" }, { age = 35, name = "Denise" }, { age = 35, name = "Helen" } ]


testInsertDuplicate : Test
testInsertDuplicate =
    test "Insert duplicate value into person-tree" <|
        \_ ->
            let
                tree =
                    insert { name = "Alice", age = 25 } personComparator personTree
            in
            Expect.equal (toList tree) [ { age = 20, name = "Charlie" }, { age = 20, name = "George" }, { age = 25, name = "Alice" }, { age = 25, name = "Eve" }, { age = 30, name = "Bob" }, { age = 30, name = "Frank" }, { age = 35, name = "Denise" }, { age = 35, name = "Helen" } ]


testDelete : Test
testDelete =
    test "Delete value from person-tree" <|
        \_ ->
            let
                tree =
                    delete { name = "Alice", age = 25 } personComparator personTree
            in
            Expect.equal (toList tree) [ { age = 20, name = "Charlie" }, { age = 20, name = "George" }, { age = 25, name = "Eve" }, { age = 30, name = "Bob" }, { age = 30, name = "Frank" }, { age = 35, name = "Denise" }, { age = 35, name = "Helen" } ]


testDeleteEmpty : Test
testDeleteEmpty =
    test "Delete from empty preson-tree" <|
        \_ ->
            let
                tree =
                    delete { name = "Alice", age = 25 } personComparator Empty
            in
            Expect.equal tree Empty


filterTest : Test
filterTest =
    test "Filter person-tree" <|
        \_ ->
            let
                filteredTree =
                    filter (\person -> person.age > 30) personComparator personTree
            in
            case countNodes filteredTree of
                2 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected a tree with 2 nodes"


mapTest : Test
mapTest =
    test "Map person-tree" <|
        \_ ->
            let
                mappedTree =
                    map (\person -> { person | age = person.age + 1 }) personTree
            in
            Expect.equal (toList mappedTree) [ { age = 21, name = "Charlie" }, { age = 21, name = "George" }, { age = 26, name = "Alice" }, { age = 26, name = "Eve" }, { age = 31, name = "Bob" }, { age = 31, name = "Frank" }, { age = 36, name = "Denise" }, { age = 36, name = "Helen" } ]


searchTest : Test
searchTest =
    test "Search element in person-tree" <|
        \_ ->
            if search { name = "Alice", age = 25 } personComparator personTree then
                Expect.pass

            else
                Expect.fail "Expected to find value { name = \"Alice\", age = 25 } in the tree"


foldlTest : Test
foldlTest =
    test "Foldl person-tree" <|
        \_ ->
            let
                sumAges =
                    foldl (\person acc -> acc + person.age) 0 personTree
            in
            Expect.equal sumAges 220


foldrTest : Test
foldrTest =
    test "Foldr person-tree" <|
        \_ ->
            let
                sumAges =
                    foldr (\person acc -> acc + person.age) 0 personTree
            in
            Expect.equal sumAges 220


mergeTest : Test
mergeTest =
    test "Merge person-trees" <|
        \_ ->
            let
                tree1 =
                    insert { name = "Alice", age = 25 } personComparator Empty
                        |> insert { name = "Bob", age = 30 } personComparator
                        |> insert { name = "Charlie", age = 20 } personComparator
                        |> insert { name = "Frank", age = 30 } personComparator
            in
            let
                tree2 =
                    insert { name = "Denise", age = 35 } personComparator Empty
                        |> insert { name = "Eve", age = 25 } personComparator
                        |> insert { name = "Frank", age = 30 } personComparator
            in
            let
                mergedTree =
                    merge tree1 tree2 personComparator
            in
            Expect.equal (toList mergedTree) [ { age = 20, name = "Charlie" }, { age = 25, name = "Alice" }, { age = 25, name = "Eve" }, { age = 30, name = "Bob" }, { age = 30, name = "Frank" }, { age = 35, name = "Denise" } ]
