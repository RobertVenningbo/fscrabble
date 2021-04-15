module internal MultiSet

    type MS<'a> when 'a : comparison = 
        | M of Map<'a, uint32>
        override x.ToString() =
            match x with
            | M m -> 
                let s = Map.fold (fun acc a b -> acc + "(" + a.ToString() + ", #" + b.ToString() + "), ") "{" m
                s.[..String.length s - 3] + "}"

    let empty                   = M Map.empty
    let isEmpty (M s)           = Map.isEmpty s
    let size (M s)              = Map.foldBack(fun k v acc -> v + acc) s 0u
    let contains a (M s)        = Map.containsKey a s
    let numItems a (M s)        = if contains a (M s) then Map.find a s else 0u
    let add a n (M s)           = if contains a (M s) then M (Map.add a ((Map.find a s) + n) s) else M (Map.add a n s)
    let addSingle a (M s)       = add a 1u (M s)
    let remove a n (M s)        = match (s.TryFind a) with
                                    | Some v -> if n >= v then M (Map.remove a s) else M (Map.add a (v - n) s)
                                    | _ -> (M s)
    let removeSingle a (M s)    = remove a 1u (M s)
    let fold f acc (M s)        = Map.fold f acc s
    let foldBack f (M s) acc    = Map.foldBack f s acc
    let map f s                 = fold (fun acc k v -> add (f k) v acc) empty s
    let ofList lst              = List.fold (fun acc x -> addSingle x acc) empty lst
    let toList s                = fold (fun acc k v -> acc@[for _ in 1..int v -> k]) [] s

    let union s1 s2             = fold (fun acc k v -> let num = numItems k acc 
                                                       if num >= v then acc 
                                                       else add k (v - num) acc) s2 s1

    let sum s1 s2               = fold (fun acc k v -> add k v acc) s1 s2
    let subtract s1 s2          = fold (fun acc k v -> remove k v acc) s1 s2

    let intersection s1 s2      = fold (fun acc k v -> let num = numItems k acc 
                                                       if num < v then acc 
                                                       else add k (v - num) acc) s2 s1

