module internal MultiSet

    type MS<'a when 'a : comparison>
    val empty : MS<'a>
    val isEmpty : MS<'a> -> bool
    val size: MS<'a> -> uint32
    val contains: 'a -> MS<'a> -> bool
    val numItems: 'a -> MS<'a> -> uint32
    val add: 'a -> uint32 -> MS<'a> -> MS<'a>
    val addSingle: 'a -> MS<'a> -> MS<'a>
    val remove: 'a -> uint32 -> MS<'a> -> MS<'a>
    val removeSingle : 'a -> MS<'a> -> MS<'a>
    val fold : ('a -> 'b -> uint32 -> 'a) -> 'a -> MS<'b> -> 'a
    val foldBack: ('a -> uint32 -> 'b -> 'b) -> MS<'a> -> 'b -> 'b
    val map: ('a -> 'b) -> MS<'a> -> MS<'b>
    val ofList: 'a list -> MS<'a>
    val toList: MS<'a> -> 'a list
    val union: MS<'a> -> MS<'a> -> MS<'a>
    val sum : MS<'a> -> MS<'a> -> MS<'a>
    val subtract: MS<'a> -> MS<'a> -> MS<'a>
    val intersection: MS<'a> -> MS<'a> -> MS<'a>