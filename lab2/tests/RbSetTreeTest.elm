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
                    insert 8 intComparator sampleTree
            in
            Expect.equal (toList tree) [ 3, 5, 8 ]


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


testRootColor : Test
testRootColor =
    test "Сheck root color" <|
        \_ ->
            let
                tree =
                    insert 1 intComparator Empty
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
                    filter (\x -> x > 4) intComparator sampleTree
            in
            case countNodes tree of
                2 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected a filtered tree with 2 nodes"



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
                    sampleTree
            in
            if search 3 intComparator tree then
                Expect.pass

            else
                Expect.fail "Expected to find value 3 in the tree"


mapTest : Test
mapTest =
    test "Map" <|
        \_ ->
            let
                tree =
                    map (\x -> x * 2) sampleTree
            in
            case toList tree of
                [ 6, 10, 16 ] ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected all values to be doubled"


foldlTest : Test
foldlTest =
    test "Foldl" <|
        \_ ->
            let
                tree =
                    sampleTree
            in
            case foldl (\x y -> x + y) 0 tree of
                16 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected sum of all values to be 16"


foldrTest : Test
foldrTest =
    test "Foldr" <|
        \_ ->
            let
                tree =
                    sampleTree
            in
            case foldr (\x y -> x + y) 0 tree of
                16 ->
                    Expect.pass

                _ ->
                    Expect.fail "Expected sum of all values to be 16"


mergeTest : Test
mergeTest =
    test "Merge" <|
        \_ ->
            let
                tree =
                    sampleTree

                merged =
                    merge (insert 4 intComparator Empty) tree intComparator
            in
            if search 5 intComparator merged then
                Expect.pass

            else
                Expect.fail "Expected to find value 5 in the merged tree"


mergeEmptyTest : Test
mergeEmptyTest =
    test "Merge with empty tree" <|
        \_ ->
            let
                tree =
                    sampleTree

                merged =
                    merge Empty tree intComparator
            in
            if search 3 intComparator merged then
                Expect.pass

            else
                Expect.fail "Expected to find value 3 in the merged tree"
