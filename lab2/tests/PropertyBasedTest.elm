module PropertyBasedTest exposing (..)

import Expect
import Fuzz exposing (list)
import Random
import RbSetTree exposing (..)
import Test exposing (Test, fuzz)
import Test exposing (fuzz2, fuzz3)


type alias Comparator a =
    a -> a -> Order


intComparator : Comparator Int
intComparator a b =
    if a < b then
        LT

    else if a > b then
        GT

    else
        EQ


fuzzRBTree : Fuzz.Fuzzer (RBTree Int)
fuzzRBTree =
    Fuzz.list (Fuzz.intRange 0 100)
        |> Fuzz.map (List.foldl (\x tree -> insert x intComparator tree) Empty)


treeIsSorted : Test
treeIsSorted =
    fuzz fuzzRBTree "Tree is sorted" <| \tree ->
        case toList tree of
            [] ->
                Expect.pass

            [ _ ] ->
                Expect.pass

            list ->
                let
                    sortedList =
                        List.sortWith intComparator list
                in
                Expect.equal list sortedList


testMerge : Test
testMerge =
    fuzz2 fuzzRBTree fuzzRBTree "Property Merge" <| \tree1 tree2 ->
        let
            gTree1 =
                insert 1 intComparator tree1
                    |> insert 2 intComparator
                    |> insert 3 intComparator
            gTree2 =
                insert 4 intComparator tree2
                    |> insert 5 intComparator
                    |> insert 6 intComparator

            merged1 =
                merge gTree1 gTree2 intComparator

            merged2 =
                merge gTree2 gTree1 intComparator
        in
        if toList merged1 == toList merged2 then
            Expect.pass

        else
            Expect.fail "Merged trees are not equal"
            

deleteTest : Test
deleteTest =
    fuzz fuzzRBTree "Property Delete" <| \tree ->
        let
            treeList =
                toList tree

            randomIndex =
                Random.step (Random.int 0 (List.length treeList - 1)) (Random.initialSeed 0)
                    |> Tuple.first

            randomValue =
                List.head (List.drop randomIndex treeList)
                    |> Maybe.withDefault 0

            treeAfterDelete =
                delete randomValue intComparator tree 
        in
        if not (search randomValue intComparator treeAfterDelete) then
            Expect.pass

        else
            Expect.fail "Value was not deleted"
        

calculateHeightTest : Test
calculateHeightTest =
    fuzz fuzzRBTree "Property Calculate Height" <| \tree ->
        let
            randomIndex =
                Random.step (Random.int 0 (List.length (toList tree) - 1)) (Random.initialSeed 0)
                    |> Tuple.first

            randomValue =
                List.head (List.drop randomIndex (toList tree))
                    |> Maybe.withDefault 0

            newTree =
                delete randomValue intComparator tree

            height =
                calculateHeight newTree
            
        in
        if height >= 0 then
            Expect.pass

        else
            Expect.fail "Height is not greater than 0"


associativityTest : Test
associativityTest =
    fuzz3 fuzzRBTree fuzzRBTree fuzzRBTree "Property Associativity" <| \tree1 tree2 tree3 ->
        let
            merged1 =
                merge (merge tree1 tree2 intComparator) tree3 intComparator

            merged2 =
                merge tree1 (merge tree2 tree3 intComparator) intComparator
        in
        if toList merged1 == toList merged2 then
            Expect.pass

        else
            Expect.fail "Merged trees are not equal"

        
emptyTest : Test
emptyTest =
    fuzz fuzzRBTree "Property Empty" <| \tree ->
        let
            emptyTree =
                Empty

            merged1 =
                merge tree emptyTree intComparator

            merged2 =
                merge emptyTree tree intComparator
        in
        if toList merged1 == toList merged2 then
            Expect.pass

        else
            Expect.fail "Merged trees are not equal"