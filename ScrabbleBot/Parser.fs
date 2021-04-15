// ScrabbleUtil contains the types coord, boardProg, and SquareProg. Remove these from your file before proceeding.
// Also note that the modulse Ass7 and ImpParser have been merged to one module called Parser.

module internal Parser

    open ScrabbleUtil // NEW. KEEP THIS LINE.
    open System
    open Eval
    open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

    let pIntToChar  = pstring "intToChar" <?> "intTochar"
    let pPointValue = pstring "pointValue" <?> "pointValue"

    let pCharToInt  = pstring "charToInt" <?> "charToInt"
    let pToUpper    = pstring "toUpper" <?> "toUpper"
    let pToLower    = pstring "toLower" <?> "toLower"
    let pCharValue  = pstring "charValue" <?> "charValue"

    let pTrue       = pstring "true" <?> "true"
    let pFalse      = pstring "false" <?> "false"
    let pIsDigit    = pstring "isDigit" <?> "isDigit"
    let pIsLetter   = pstring "isLetter" <?> "isLetter"

    let pif       = pstring "if" <?> "if"
    let pthen     = pstring "then" <?> "then"
    let pelse     = pstring "else" <?> "else"
    let pwhile    = pstring "while" <?> "while"
    let pdo       = pstring "do" <?> "do"
    let pdeclare  = pstring "declare" <?> "declare"

    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
    let pletter        = satisfy System.Char.IsLetter
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit

    let spaces         = many whitespaceChar
    let spaces1        = many1 whitespaceChar

    let (.>*>.) p1 p2 =  p1 .>> spaces .>>. p2 
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 >>. spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> spaces .>*> pchar ')'
    let curly        p = pchar '{' >*>. p .>*> pchar '}'

    let pid = 
        let aux str = List.ofSeq str |> List.toArray |> System.String
        (pchar '_' <|> pletter) .>>. ((palphanumeric <|> pchar '_') |> many) |>> fun (x, y) -> aux (x::y)

    // he parser many : Parser<'a> -> Parser<'a list> takes a parser p and returns a
    // parser that parses 0 or more occurrences of whatever p parses and puts the result in a list.

    let unop op a = op >*>. a 
    let binop op p1 p2 = (p1 .>*> op) .>*>. p2

    let TermParse, tref = createParserForwardedToRef<aExp>()
    let ProdParse, pref = createParserForwardedToRef<aExp>()
    let AtomParse, aref = createParserForwardedToRef<aExp>()
    let cTermParse, cref = createParserForwardedToRef<cExp>()
    let b1TermParse, b1ref = createParserForwardedToRef<bExp>()
    let b2TermParse, b2ref = createParserForwardedToRef<bExp>()
    let b3TermParse, b3ref = createParserForwardedToRef<bExp>()
    let SstmntParse, sref = createParserForwardedToRef<stm>()
    let SstmntParse2, sref2 = createParserForwardedToRef<stm>()

    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"

    do tref := choice [AddParse; SubParse; ProdParse]


    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"

    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul (N -1, x)) <?> "Neg"   
    let PVParse = unop pPointValue AtomParse |>> PV <?> "PV"
    let VParse   = pid |>> V <?> "V"
    let NParse   = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let charToIntParse = unop pCharToInt cTermParse |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; PVParse; charToIntParse; ParParse; NParse; VParse]

    let AexpParse = TermParse 

    let cParse = pchar ''' >>. (palphanumeric <|> whitespaceChar) .>> pchar ''' |>> C <?> "C"
    let CVParse = unop pCharValue AtomParse |>> CV <?> "CV"
    let ITCParse = unop pIntToChar AtomParse |>> IntToChar <?> "IntToChar"
    let toUpperParse = unop pToUpper (parenthesise cTermParse) |>> ToUpper <?> "ToUpper"
    let toLowerParse = unop pToLower (parenthesise cTermParse) |>> ToLower <?> "ToLower"
    let cParParse = parenthesise cTermParse
    let CexpParse = cTermParse
    do cref := choice [cParse; CVParse; ITCParse; toUpperParse; toLowerParse; cParParse]

    let BexpParse = b1TermParse
    
    let conjParse = binop (pstring "/\\") b2TermParse b1TermParse |>> (fun (x, y) -> (.&&.) x y) <?> "Conj"
    let disParse =  binop (pstring "\\/") b2TermParse b1TermParse |>> (fun (x, y) -> (.||.) x y) <?> "Disj"
    do b1ref := choice [conjParse; disParse; b2TermParse] 

    let EqParse = binop (pchar '=') TermParse TermParse |>> AEq <?> "AEq"
    let NotEqParse = binop (pstring "<>") TermParse TermParse |>> (fun (x,y) -> Not(AEq(x,y))) <?> "NotEqual"
    let LTParse = binop (pchar '<') TermParse TermParse |>> ALt <?> "ALt"
    let LTEqParse = binop (pstring "<=") TermParse TermParse |>> (fun (x,y) -> x .<=. y)
    let GTParse = binop (pchar '>') TermParse TermParse |>> (fun (x,y) -> x .>. y)
    let GTEqParse = binop (pstring ">=") TermParse TermParse |>> (fun (x,y) -> x .>=. y)
    do b2ref := choice [EqParse; NotEqParse; LTParse; LTEqParse; GTParse; GTEqParse; b3TermParse]

    let NotParse = unop (pchar '~') b3TermParse |>> (fun (x) -> (~~) x) <?> "Not" 
    let LetterParse = unop pIsLetter cParse |>> IsLetter <?> "IsLetter"
    let IsDigitParse = unop pIsDigit cParse |>> IsDigit  <?> "IsDigit"
    let TrueParse = pTrue |>> (fun _ -> TT)
    let FalseParse = pFalse |>> (fun _ -> FF)
    let BParParse = parenthesise b1TermParse
    do b3ref := choice [TrueParse; FalseParse; BParParse; NotParse; LetterParse; IsDigitParse] 

    let stmntParse = SstmntParse
 
    let SeqParse = binop (pchar ';') SstmntParse2 SstmntParse |>> Seq <?> "Seq"
    do sref := choice [SeqParse; SstmntParse2]
 
    let DeclareParse = unop (pdeclare .>>. spaces1) pid |>> Declare <?> "Declare"
    let curlyParse = curly SstmntParse
    let AssParse = binop (pstring ":=") pid TermParse |>> Ass <?> "Ass"
    let ITParse = unop pif BParParse .>*>. unop pthen curlyParse |>> (fun (x, y) -> ITE(x, y, Skip)) <?> "If-Then"
    let ITEParse = unop pif BParParse .>*>. unop pthen curlyParse .>*>. unop pelse curlyParse |>> (fun ((x, y), z) -> ITE(x, y, z)) <?> "If-Then-Else"
    let WhileParse = unop pwhile BParParse .>*>. unop pdo curlyParse |>> While <?> "While"
    do sref2 := choice [DeclareParse; AssParse; ITEParse; ITParse; WhileParse]

    (* These five types will move out of this file once you start working on the project *)
    type coord      = int * int
    type squareProg = Map<int, string>
    //type boardProg  = {
    //        prog       : string;
    //        squares    : Map<int, squareProg>
    //        usedSquare : int
    //        center     : coord
    //        isInfinite : bool   // For pretty-printing purposes only
    //        ppSquare   : string // For pretty-printing purposes only
    //    }

    type word   = (char * int) list
    type square = Map<int, word -> int -> int -> int>

    let parseSquareFun (squareProgs:squareProg) : square = Map.map (fun _ s -> run stmntParse s |> (fun x -> stmntToSquareFun (getSuccess x))) squareProgs

    type boardFun = coord -> square option

    let parseBoardFun s m = run stmntParse s |> (fun x -> stmntToBoardFun (getSuccess x) m)

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let parseBoardProg (BP:boardProg) = {
        center = BP.center;
        defaultSquare = (Map.map (fun k v -> (parseSquareFun v)) BP.squares) |> Map.find BP.usedSquare;
        squares = parseBoardFun BP.prog (Map.map (fun k v -> parseSquareFun v) BP.squares)
    }