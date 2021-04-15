module internal Eval

    open StateMonad

    (* Code for testing *)

    let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)]
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b = a >>= (fun x -> b >>= (fun y -> ret (x + y)))
    let div a b = a >>= (fun x -> b >>= (fun y -> if y <> 0 then ret (x / y) else fail DivisionByZero))

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> = 
        match a with
        | N n -> ret n
        | V x -> lookup x
        | WL -> wordLength
        | Add(a1, a2) -> arithEval a1 >>= fun x -> arithEval a2 >>= fun y -> ret (x + y)
        | Sub(a1, a2) -> arithEval a1 >>= (fun x -> arithEval a2 >>= (fun y -> ret (x - y)))
        | Mul(a1, a2) -> arithEval a1 >>= (fun x -> arithEval a2 >>= (fun y -> ret (x * y)))
        | Mod (a1, a2) -> 
            arithEval a1 >>= fun x -> 
            arithEval a2 >>= fun y -> 
            if y <> 0 then ret (x % y) else fail DivisionByZero
        | Div (a1, a2) -> 
             arithEval a1 >>= fun x -> 
             arithEval a2 >>= fun y -> 
             if y <> 0 then ret (x / y) else fail DivisionByZero
        | PV pv -> arithEval pv >>= fun x -> pointValue x
        | CharToInt c -> charEval c >>= fun c -> ret (int c)

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | CV cv -> arithEval cv >>= fun x -> characterValue x
        | ToUpper t -> charEval t >>= fun x -> ret (System.Char.ToUpper x)
        | ToLower t -> charEval t >>= fun x -> ret (System.Char.ToLower x)
        | IntToChar t -> arithEval t >>= fun t -> ret (char t)

    and boolEval b : SM<bool> = 
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (a1, a2) -> arithEval a1 >>= fun x -> arithEval a2 >>= fun y -> if x = y then boolEval TT else boolEval FF
        | ALt (a1, a2) -> arithEval a1 >>= fun x -> arithEval a2 >>= fun y -> if x < y then boolEval TT else boolEval FF
        | Not a -> boolEval a >>= fun x -> ret (not x)
        | Conj (a1, a2) -> boolEval a1 >>= fun x -> boolEval a2 >>= fun y -> if x && y then boolEval TT else boolEval FF
        | IsLetter c -> charEval c >>= fun c -> ret (System.Char.IsLetter c)
        | IsDigit c -> charEval c >>= fun c -> ret (System.Char.IsNumber c)

    type stm =                (* statements *)
    | Declare of string       (* variable declaration *)
    | Ass of string * aExp    (* variable assignment *)
    | Skip                    (* nope *)
    | Seq of stm * stm        (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else statement *)
    | While of bExp * stm     (* while statement *)

    let rec stmntEval stmnt : SM<unit> = 
        match stmnt with
        | Declare str -> declare str
        | Ass (str, a) -> arithEval a >>= fun b -> update str b
        | Skip -> ret ()
        | Seq (stm1, stm2) -> stmntEval stm1 >>>= stmntEval stm2
        | ITE (b, stm1, stm2) -> boolEval b >>= fun x -> push >>>= (if x then stmntEval stm1 else stmntEval stm2) >>>= pop
        | While (b, stm) -> boolEval b >>= fun x -> push >>>= (if x then stmntEval stm >>>= stmntEval (While (b, stm)) else ret ()) >>>= pop

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"
    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 *) 

    type word = (char * int) list
    type squareFun = word -> int -> int -> int

    let stmntToSquareFun stm : word -> int -> int -> int = 
        fun w pos acc -> stmntEval stm >>>= lookup "_result_" |> evalSM (mkState [("_pos_", pos); ("_result_", 0); ("_acc_", acc)] w ["_pos_"; "_result_"; "_acc_"]) 
                            |> fun result -> match result with
                                                | Success id -> id
                                                | Failure _ -> 0

    //type coord = int * int

    //type boardFun = coord -> Result<squareFun option, Error> 

    let stmntToBoardFun s (m : Map<int, 'a>) = 
        fun (x, y) -> 
            stmntEval s >>>= lookup "result" 
            |> evalSM (mkState [("x", x); ("y", y); ("result", 0)] [] ["x"; "y"; "result"]) 
            |> fun result -> match result with
                                | Success id -> match m.TryFind id with
                                                | Some s -> Some s
                                                | None -> None
                                | Failure f -> None

    //type board = {
    //    center        : coord
    //    defaultSquare : squareFun
    //    squares       : boardFun
    //}

    //let mkBoard (c : coord) (defaultSq : stm) (boardStmnt : stm ) (ids : list<int * stm>) = { 
    //    center = c; 
    //    defaultSquare = stmntToSquareFun defaultSq; 
    //    squares = stmntToBoardFun boardStmnt (List.map (fun (k, sq) -> (k, stmntToSquareFun sq)) ids |> Map.ofList)  
    //}