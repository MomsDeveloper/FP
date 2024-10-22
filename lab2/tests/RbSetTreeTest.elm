module RbSetTreeTest exposing (..)

import Expect
import RbSetTree exposing (..)
import Test exposing (Test, test)


intComparator : Comparator Int
intComparator a b =
    if a < b then
        LT

    else if a > b then
        GT

    else
        EQ


sampleTree : RBTree Int
sampleTree =
    insert 5 intComparator Empty
        |> insert 3 intComparator
        |> insert 8 intComparator


testInsertEmpty : Test
testInsertEmpty =
    test "Insert into empty tree" <|
        \_ ->
            let
                tree =
                    insert 10 intComparator Empty
            in
            case tree of
                Node _ _ value _ ->
                    Expect.equal value 10

                _ ->
                    Expect.fail "Expected a tree with a single node"


testInsertMultiple : Test
testInsertMultiple =
    test "Insert multiple values" <|
        \_ ->
            let
                tree =
                    insert 8 intComparator (insert 3 intComparator (insert 5 intComparator Empty))
            in
            Expect.equal (toList tree) [ 3, 5, 8 ]


testInsertDuplicate : Test
testInsertDuplicate =
    test "Insert duplicate value" <|
        \_ ->
            let
                tree =
                    insert 2 intComparator <| insert 9 intComparator (insert 4 intComparator sampleTree)
            in
            Expect.equal (toList tree) [ 2, 3, 4, 5, 8, 9 ]


testColorsToList : Test
testColorsToList =
    test "Colors to list" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree
            in
            Expect.equal (colorsToList tree) [ Black, Black, Black, Red ]


testDelete : Test
testDelete =
    test "Delete" <|
        \_ ->
            let
                tree =
                    delete 5 intComparator sampleTree
            in
            Expect.equal (toList tree) [ 3, 8 ]


testDeleteEmpty : Test
testDeleteEmpty =
    test "Delete from empty tree" <|
        \_ ->
            let
                tree =
                    delete 5 intComparator Empty
            in
            Expect.equal tree Empty


testDeleteCase1 : Test
testDeleteCase1 =
    test "Delete case 1" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator |> insert 4 intComparator |> delete 3 intComparator
            in
            case tree of
                Node _ (Node color _ _ _) _ (Node color2 _ _ _) ->
                    Expect.equal color color2

                _ ->
                    Expect.fail "Expected nodes to have same color"


testDeleteCase2 : Test
testDeleteCase2 =
    test "Delete case 2" <|
        \_ ->
            let
                tree =
                    insert 4 intComparator sampleTree |> insert 1 intComparator |> delete 4 intComparator
            in
            case tree of
                Node _ _ value _ ->
                    Expect.equal value 5

                _ ->
                    Expect.fail "Expected root node with value 5"


testDeleteCase3 : Test
testDeleteCase3 =
    test "Delete case 3" <|
        \_ ->
            let
                tree =
                    insert 6 intComparator sampleTree |> insert 9 intComparator |> delete 5 intComparator
            in
            case tree of
                Node _ (Node _ Empty value Empty) _ _ ->
                    Expect.equal value 3

                _ ->
                    Expect.fail "Expected value 3"


testRootColor : Test
testRootColor =
    test "Root color" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator |> insert 4 intComparator |> delete 2 intComparator
            in
            case tree of
                Node color _ _ _ ->
                    Expect.equal color Black

                _ ->
                    Expect.fail "Expected Black root node"


filterTest : Test
filterTest =
    test "Filter" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator |> filter (\x -> x > 4) intComparator
            in
            case countNodes tree of
                2 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected a tree with 2 nodes"



filterEmptyTest : Test  
filterEmptyTest =
    test "Filter empty tree" <|
        \_ ->
            let
                tree =
                    filter (\x -> x > 4) intComparator Empty
            in
            case countNodes tree of
                0 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected an empty tree"


searchTest : Test
searchTest =
    test "Search" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator
            in
            if search 2 intComparator tree then
                Expect.pass

            else
                Expect.fail "Expected to find value 2 in the tree"


mapTest : Test
mapTest =
    test "Map" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator |> map (\x -> x * 2)
            in
            case toList tree of
                [ 2, 4, 6, 10, 16 ] ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected all values to be doubled"


foldlTest : Test
foldlTest =
    test "Foldl" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator
            in
            case foldl (\x y -> x + y) 0 tree of
                19 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected sum of all values to be 19"


foldrTest : Test
foldrTest =
    test "Foldr" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator
            in
            case foldr (\x y -> x + y) 0 tree of
                19 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected sum of all values to be 19"


mergeTest : Test
mergeTest =
    test "Merge" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator

                merged =
                    merge (insert 4 intComparator Empty) tree intComparator
            in
            if search 5 intComparator merged then
                Expect.pass

            else
                Expect.fail "Expected to find value 4 in the tree"


mergeEmptyTest : Test
mergeEmptyTest =
    test "Merge with empty tree" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator sampleTree |> insert 2 intComparator |> insert 3 intComparator

                merged =
                    merge Empty tree intComparator
            in
            if search 3 intComparator merged then
                Expect.pass

            else
                Expect.fail "Expected to find value 3 in the tree"

            
mergeCase1Test : Test
mergeCase1Test =
    test "Merge case 1" <|
        \_ ->
            let
                expectedTree =
                    insert 1 intComparator sampleTree |> insert 4 intComparator |> insert 3 intComparator
                tree2 =
                    insert 4 intComparator Empty |> insert 3 intComparator |> insert 1 intComparator

                merged =
                    merge sampleTree tree2 intComparator
            in
            Expect.equal (toList merged) (toList expectedTree)