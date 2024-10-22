module InfiniteList exposing (..)


type Ctx a
    = InitCtx
    | NextCtx a


type Yield a
    = Value a
    | Stop

type alias Generator ctx yield =
    Ctx ctx -> ( Ctx ctx, Yield yield )


fromList : List a -> Generator (List a) a
fromList list =
    \ctx ->
        case ctx of
            InitCtx ->
                case list of
                    [] ->
                        ( InitCtx, Stop )

                    x :: xs ->
                        ( NextCtx xs, Value x )

            NextCtx xs ->
                case xs of
                    [] ->
                        ( InitCtx, Stop )

                    x :: xs_ ->
                        ( NextCtx xs_, Value x )


range : { start : Int, step : Int, stop : Int } -> Generator Int Int
range { start, step, stop } =
    \ctx ->
        case ctx of
            InitCtx ->
                if start < stop then
                    ( NextCtx start, Value start )

                else
                    ( InitCtx, Stop )

            NextCtx n ->
                if n + step < stop then
                    ( NextCtx (n + step), Value (n + step) )

                else
                    ( InitCtx, Stop )


map : (a -> b) -> Generator ctx a -> Generator ctx b
map f gen =
    \ctx ->
        let
            ( ctx_, yield ) =
                gen ctx
        in
        case yield of
            Value r ->
                ( ctx_, Value (f r) )

            Stop ->
                ( InitCtx, Stop )


filter : (a -> Bool) -> Generator ctx a -> Generator ctx a
filter pred gen =
    \ctx ->
        let
            ( ctx_, yield ) =
                gen ctx
        in
        case yield of
            Value r ->
                if pred r then
                    ( ctx_, Value r )

                else
                    ctx_ |> filter pred gen

            Stop ->
                ( InitCtx, Stop )


fold : (acc -> a -> acc) -> acc -> Generator ctx a -> acc
fold f acc gen =
    let
        aux ctx acc_ =
            let
                ( ctx_, yield ) =
                    gen ctx
            in
            case yield of
                Value r ->
                    aux ctx_ (f acc_ r)

                Stop ->
                    acc_
    in
    aux InitCtx acc


take : Int -> Generator ctx r -> List r
take n gen =
    let
        aux n_ ctx acc =
            if n_ <= 0 then
                List.reverse acc

            else
                let
                    ( ctx_, yield ) =
                        gen ctx
                in
                case yield of
                    Value r ->
                        aux (n_ - 1) ctx_ (r :: acc)

                    Stop ->
                        List.reverse acc
    in
    aux n InitCtx []
