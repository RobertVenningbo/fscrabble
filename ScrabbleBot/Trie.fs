module Trie

type BTrie =
    | Node of bool
            * BTrie option
            * BTrie option

let empty = Node (false, None, None)

type Digit = D0 | D1

let rec splitDigit : uint32 -> (Digit * uint32) =
    fun x ->
    let y = x / 2u
    match x % 2u with
    | 0u -> D0 , y
    | _ -> D1 , y

let rec digits : uint32 -> Digit list = fun x ->
    let (d, y) = splitDigit x
    d :: if y = 0u then [] else digits y

let orEmpty =
    function
    | Some t -> t
    | None   -> empty

let insert x t =
    let rec go =
        function
        | ([], Node (b,l,r)) -> Node(true,l,r)
        | (d::ds, Node(b,l,r)) ->
            match d with
            |D0 -> Node(b, Some (go (ds,l |> orEmpty)), r)
            |D1 -> Node(b, l, Some (go (ds,r |> orEmpty)))
    go (digits x, t)


let lookup x t =
    let rec go =
        function 
        |([] , Node(b,_,_)) -> b
        |((d::ds), Node (_,l,r)) ->
        let next = match d with
                    | D0 -> l
                    | D1 -> r
        match next with
        | Some t' -> go (ds,t')
        | None -> false
    go(digits x, t)

type Dict = Node of Map<char, bool * Dict>