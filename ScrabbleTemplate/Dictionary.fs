// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

    type Dict = Node of Map<char, bool * Dict> // Not implemented

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty () = Node Map.empty
    let lookup (s:string) dict = 
        let rec aux charlist (Node d) =
            match charlist with
            | [x] ->    
                match Map.tryFind x d with 
                | Some (b,_) -> b
                | None -> false
            | x::xs -> 
                    match Map.tryFind x d with 
                    | Some (_,d) -> aux xs d
                    | None -> false   
            |[] -> false //so compiler isnt a bitch
        aux (Array.toList (s.ToCharArray () )) dict

    let insert (s:string) dict = 
        let rec aux charlist (Node d) =
            match charlist with
            |[x] -> 
                match Map.tryFind x d with
                |Some (_,d')    -> Map.add x (true, d') d
                |None           -> Map.add x (true, empty ()) d
            |x::xs -> 
                match Map.tryFind x d with
                |Some (b,d')    -> Map.add x (b,Node(aux xs d')) d
                |None           -> Map.add x (false, Node(aux xs (empty ()))) d
        Node(aux (Array.toList (s.ToCharArray () )) dict)

    let step c (Node d) = Map.tryFind c d 


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"