module CardConverter

open Model

module CardRank =
    let fromString s =
        match s with
        | "A" -> HighAce
        | "K" -> King
        | "Q" -> Queen
        | "J" -> Jack
        | "10" -> Ten
        | "9" -> Nine
        | "8" -> Eight
        | "7" -> Seven
        | "6" -> Six
        | "5" -> Five
        | "4" -> Four
        | "3" -> Three
        | "2" -> Two
        | _ -> failwith "Rank.FromString"

module Suit =
    let fromString c =
        match c with
        | "S" -> Spades
        | "C" -> Clubs
        | "D" -> Diamands
        | "H" -> Hearts
        | _ -> failwith "Suit.FromString"

module Card =
    let fromString(s: string) =
        let l = String.length s
        (s.[0..l - 2],s.[l - 1..l - 1]) 
        |> fun (rank,suit) -> 
            (CardRank.fromString rank,Suit.fromString suit) |> Card

module Hand =
    let parseHand(s: string) =
        s.Split [|' '|]
        |> Array.map Card.fromString
        |> Array.toList
        |> Hand
