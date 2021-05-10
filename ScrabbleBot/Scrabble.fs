namespace scrbbl

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open System.IO
open ScrabbleUtil.DebugPrint

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?" 
        Regex.Matches(ts, pattern) |>
        Seq.cast<Match> |> 
        Seq.map 
            (fun t -> 
                match t.Value with
                | Regex pattern [x; y; id; c; p] ->
                    ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
                | _ -> failwith "Failed (should never happen)") |>
        Seq.toList

 module Print =

    let printHand pieces hand =
        hand |>
        MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State = 
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    type state = {
        board         : Parser.board
        dict          : ScrabbleUtil.Dictionary.Dict
        playerNumber  : uint32
        hand          : MultiSet.MS<uint32>
        tilesPlaced   : Map<coord, char*int>
    }

    let mkState b d pn h t= {board = b; dict = d;  playerNumber = pn; hand = h; tilesPlaced = t }

    let board st         = st.board
    let dict st          = st.dict
    let playerNumber st  = st.playerNumber
    let hand st          = st.hand

module Scrabble =
    open System.Threading

    let findPlay (hand:MultiSet.MS<uint32>) (dictionary:Dictionary.Dict) (tiles:Map<uint32, tile>) = 
        
        let keyToPlayString (key:uint32) = 
            (Map.find(key) tiles) |> Set.toList |> fun x -> x.[0] |> fun (c, v) -> string key + string c + string v

        let keyToChar (key:uint32) = 
            Map.find(key) tiles |> Set.toList |> fun x -> x.[0] |> fun (c, _) -> c

        let rec aux (hand:MultiSet.MS<uint32>) (subDictionary:Dictionary.Dict) =
            List.fold(fun acc x -> 
                if List.isEmpty acc then 
                    if Dictionary.lookup (string (keyToChar x)) subDictionary then x::acc
                    else
                        match Dictionary.step (keyToChar x) subDictionary with
                            | Some robert -> 
                                debugPrint (sprintf "yeeehonk") // keep the debug lines. They are useful.
                                let check = aux (MultiSet.removeSingle x hand) subDictionary
                                if check.IsEmpty then acc else x::check@acc
                            | None -> acc
                else acc
            ) List.empty<uint32> (MultiSet.toList hand)

        let playKeys = aux hand dictionary
        List.fold(fun acc key -> acc + "0 " + string (List.findIndex(fun x -> x = key) playKeys) + " " + keyToPlayString key + " ") "" playKeys

    let playGame cstream pieces (st : State.state) =

        let rec aux (st : State.state) =
            Print.printHand pieces (State.hand st)

            // remove the force print when you move on from manual input (or when you have learnt the format)
            forcePrint "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
            let input =  System.Console.ReadLine()
             
            let word = findPlay st.hand st.dict pieces
            let move = RegEx.parseMove word

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM (CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //let hand = State.hand st
                //let board = State.board st      
                //let dict = State.dict st 
                //let playerNumber = State.playerNumber st
                let tilesPlaced = List.fold(fun acc (coord,(_,tile)) -> Map.add coord tile acc) st.tilesPlaced ms

                let fakeHand (*actually the hand after removed pieces*) = List.fold(fun acc (_,(important,_)) -> MultiSet.removeSingle important acc) st.hand ms
                let yesHand (*this is the new hand*) = List.fold(fun acc (a,_) -> MultiSet.addSingle a acc) fakeHand newPieces

                let st' = {st with hand = yesHand; tilesPlaced = tilesPlaced} // This state needs to be updated
                aux st'
            | RCM (CMPlayed (pid, ms, points)) ->
                (* Successful play by other player. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMPlayFailed (pid, ms)) ->
                (* Failed play. Update your state *)
                let st' = st // This state needs to be updated
                aux st'
            | RCM (CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


        aux st

    let startGame 
            (boardP : boardProg) 
            (dictf : bool -> Dictionary.Dict) 
            (numPlayers : uint32) 
            (playerNumber : uint32) 
            (playerTurn  : uint32) 
            (hand : (uint32 * uint32) list)
            (tiles : Map<uint32, tile>)
            (timeout : uint32 option) 
            (cstream : Stream) =
        debugPrint 
            (sprintf "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n" numPlayers playerNumber playerTurn hand timeout)

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary
        let board = Parser.parseBoardProg boardP
                  
        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () -> playGame cstream tiles (State.mkState board dict playerNumber handSet Map.empty)
        
