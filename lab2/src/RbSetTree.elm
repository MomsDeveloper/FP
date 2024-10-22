module RbSetTree exposing (Color(..), RBTree(..), Comparator, insert, blacken, redden, balance, delete, removeMin, rotate, toList, countNodes, colorsToList, search, filter, map, foldl, foldr, merge, calculateHeight)


type Color
    = Red
    | Black
    | DoubleBlack


type RBTree a
    = Empty
    | DoubleEmpty
    | Node Color (RBTree a) a (RBTree a)


type alias Comparator a =
    a -> a -> Order


insert : a -> Comparator a -> RBTree a -> RBTree a
insert value compare tree =
    let
        aux node =
            case node of
                Node color left v right ->
                    case compare value v of
                        LT ->
                            balance (Node color (aux left) v right)

                        GT ->
                            balance (Node color left v (aux right))

                        EQ ->
                            node

                _ ->
                    Node Red Empty value Empty
    in
    blacken (aux tree)


blacken : RBTree a -> RBTree a
blacken node =
    case node of
        Node _ left v right ->
            Node Black left v right

        _ ->
            node


redden : RBTree a -> RBTree a
redden node =
    case node of
        Node _ left v right ->
            Node Red left v right

        _ ->
            node


balance : RBTree a -> RBTree a
balance tree =
    case tree of
        Node Black (Node Red (Node Red a x b) y c) z d ->
            Node Red (Node Black a x b) y (Node Black c z d)

        Node Black (Node Red a x (Node Red b y c)) z d ->
            Node Red (Node Black a x b) y (Node Black c z d)

        Node Black a x (Node Red b y (Node Red c z d)) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        Node Black a x (Node Red (Node Red b y c) z d) ->
            Node Red (Node Black a x b) y (Node Black c z d)

        Node DoubleBlack a x (Node Red (Node Red b y c) z d) ->
            Node Black (Node Black a x b) y (Node Black c z d)

        Node DoubleBlack (Node Red a x (Node Red b y c)) z d ->
            Node Black (Node Black a x b) y (Node Black c z d)

        _ ->
            tree


delete : a -> Comparator a -> RBTree a -> RBTree a
delete value compare tree =
    let
        aux node =
            case node of
                Empty ->
                    Empty

                Node Red Empty y Empty ->
                    case compare value y of
                        EQ ->
                            Empty

                        _ ->
                            node

                Node Black Empty y Empty ->
                    case compare value y of
                        EQ ->
                            DoubleEmpty

                        _ ->
                            node

                Node Black (Node Red Empty y Empty) z Empty ->
                    case compare value z of
                        LT ->
                            Node Black (aux (Node Red Empty y Empty)) z Empty

                        EQ ->
                            Node Black Empty y Empty

                        GT ->
                            node

                Node c a y b ->
                    case compare value y of
                        LT ->
                            rotate (Node c (aux a) y b)

                        EQ ->
                            case removeMin b of
                                Just (minRight, newRight) ->
                                    rotate (Node c a minRight newRight)

                                Nothing ->
                                    node

                        GT ->
                            rotate (Node c a y (aux b))

                _ ->
                    node
    in
    blacken (aux tree)


removeMin : RBTree a -> Maybe (a, RBTree a)
removeMin node =
    case node of
        Node Red Empty x Empty ->
            Just (x, Empty)

        Node Black Empty x Empty ->
            Just (x, DoubleEmpty)

        Node Black Empty x (Node Red Empty y Empty) ->
            Just (x, Node Black Empty y Empty)

        Node c a x b ->
            case removeMin a of
                Just (minLeft, newLeft) ->
                    Just (minLeft, rotate (Node c newLeft x b))

                _ ->
                    Nothing

        Empty ->
            Nothing

        DoubleEmpty ->
            Nothing

rotate : RBTree a -> RBTree a
rotate tree =
    case tree of
        Node Red (Node DoubleBlack a x b) y (Node Black c z d) ->
            balance (Node Black (Node Red (Node Black a x b) y c) z d)

        Node Red DoubleEmpty y (Node Black c z d) ->
            balance (Node Black (Node Red Empty y c) z d)

        Node Red (Node Black a x b) y (Node DoubleBlack c z d) ->
            balance (Node Black a x (Node Red b y (Node Black c z d)))

        Node Red (Node Black a x b) y DoubleEmpty ->
            balance (Node Black a x (Node Red b y Empty))

        Node Black (Node DoubleBlack a x b) y (Node Black c z d) ->
            balance (Node DoubleBlack (Node Red (Node Black a x b) y c) z d)

        Node Black DoubleEmpty y (Node Black c z d) ->
            balance (Node DoubleBlack (Node Red Empty y c) z d)

        Node Black (Node Black a x b) y (Node DoubleBlack c z d) ->
            balance (Node DoubleBlack a x (Node Red b y (Node Black c z d)))

        Node Black (Node Black a x b) y DoubleEmpty ->
            balance (Node DoubleBlack a x (Node Red b y Empty))

        Node Black (Node DoubleBlack a w b) x (Node Red (Node Black c y d) z e) ->
            Node Black (balance (Node Black (Node Red (Node Black a w b) x c) y d)) z e

        Node Black DoubleEmpty x (Node Red (Node Black c y d) z e) ->
            Node Black (balance (Node Black (Node Red Empty x c) y d)) z e

        Node Black (Node Red a w (Node Black b x c)) y (Node DoubleBlack d z e) ->
            Node Black a w (balance (Node Black b x (Node Red c y (Node Black d z e))))

        Node Black (Node Red a w (Node Black b x c)) y DoubleEmpty ->
            Node Black a w (balance (Node Black b x (Node Red c y Empty)))

        _ ->
            tree


toList : RBTree a -> List a
toList tree =
    case tree of
        Empty ->
            []

        DoubleEmpty ->
            []

        Node _ left value right ->
            toList left ++ (value :: toList right)


countNodes : RBTree a -> Int
countNodes tree =
    case tree of
        Empty ->
            0

        DoubleEmpty ->
            0

        Node _ left _ right ->
            1 + countNodes left + countNodes right


colorsToList : RBTree a -> List Color
colorsToList tree =
    case tree of
        Empty ->
            []

        DoubleEmpty ->
            []

        Node color left _ right ->
            colorsToList left ++ (color :: colorsToList right)


search : a -> Comparator a -> RBTree a -> Bool
search value compare tree =
    let
        aux node =
            case node of
                Empty ->
                    False

                DoubleEmpty ->
                    False

                Node _ left v right ->
                    case compare value v of
                        LT ->
                            aux left

                        GT ->
                            aux right

                        EQ ->
                            True
    in
    aux tree


filter : (a -> Bool) -> Comparator a -> RBTree a -> RBTree a
filter predicate compare tree =
    let
        aux node =
            case node of
                Empty ->
                    Empty

                DoubleEmpty ->
                    DoubleEmpty

                Node color left value right ->
                    if predicate value then
                        Node color (aux left) value (aux right)

                    else
                        delete value compare node |> aux
    in
    aux tree


map : (a -> b) -> RBTree a -> RBTree b
map func tree =
    case tree of
        Empty ->
            Empty

        DoubleEmpty ->
            Empty

        Node color left value right ->
            Node color (map func left) (func value) (map func right)


foldl : (a -> b -> b) -> b -> RBTree a -> b
foldl func acc tree =
    case tree of
        Empty ->
            acc

        DoubleEmpty ->
            acc

        Node _ left value right ->
            foldl func (func value (foldl func acc left)) right


foldr : (a -> b -> b) -> b -> RBTree a -> b
foldr func acc tree =
    case tree of
        Empty ->
            acc

        DoubleEmpty ->
            acc

        Node _ left value right ->
            foldr func (func value (foldr func acc right)) left


merge : RBTree a -> RBTree a -> Comparator a -> RBTree a
merge tree1 tree2 compare =
    List.foldl (\x tree -> insert x compare tree) tree1 (toList tree2)


calculateHeight : RBTree a -> Int
calculateHeight tree =
    case tree of
        Empty ->
            1

        DoubleEmpty ->
            0

        Node color left _ right ->
            let
                leftHeight =
                    calculateHeight left

                rightHeight =
                    calculateHeight right

                currentHeight =
                    if color == Black then
                        1

                    else
                        0
            in
            if leftHeight == rightHeight then
                leftHeight + currentHeight

            else
                -1
