module Poker

open Model
open CardConverter

// let findHighCard cards =
//     cards
//     |> List.map(fun (r,_cs) -> r)
//     |> List.max
// let (|HighCard|_|)(Hand hand) =
//     hand
//     |> List.sortByDescending(fun (r,_cs) -> r)
//     |> HandRank.HighCard
//     |> Some
let remainingRanks =
    let inner cards =
        cards
        |> List.map fst
        |> function 
        | [] -> None
        | rs -> Some rs
    inner

let (|HighCard|_|)(Hand cards) =
    remainingRanks cards |> Option.map HandRank.HighCard

let findOfKindRanks n =
    let inner cards = 
        cards
        |> List.groupBy(fun (r,cs) -> r)
        |> List.sortByDescending(fun (r,cs) -> r)
        |> List.tryFind(fun (r,cs) -> List.length cs = n)
        |> Option.map(fun (r,cs) -> 
               let remaining =
                   cards
                   |> List.except cs
                   |> List.sortByDescending(fun (r,_cs) -> r)
               cs,remaining)
    inner
let (|Pair|_|)(Hand hand) =
    findOfKindRanks |> |> Option.map HandRank.Pair
// let (|Pair|_|)(Hand hand) =
//     hand
//     |> findOfKindRanks 2
//     |> fun cs -> 
//         match cs with
//         | Some(p,cs) -> HandRank.Pair(OfKind p,cs) |> Some
//         | _ -> None
// let (|TwoPair|_|)(Hand hand) =
//     let rec loop cards acc =
//         cards
//         |> findOfKindRanks 2
//         |> fun cs -> 
//             match cs with
//             | Some(p,remainingCards) -> 
//                 if List.length acc = 2 then 
//                     HandRank.TwoPair(OfKind acc,OfKind p,cards) |> Some
//                 else loop remainingCards p
//             | _ -> None
//     loop hand []
//     hand
//     |> findOfKindRanks 2
//     |> fun rs -> 
//         match rs with
//         | [_;_] -> 
//             rs
//             |> List.max
//             |> Some
//         | _ -> None
// let (|TreeOfKind|_|)(Hand hand) =
//     hand
//     |> findOfKindRanks 3
//     |> fun rs -> 
//         match rs with
//         | [r] -> r |> Some
//         | _ -> None
let toHandRank hand =
    match hand with
    //     | StraightFlush r
    //     | FourOfKind r
    //     | FullHouse r
    //     | Flush r
    //| Straight r
    //| TreeOfKind r -> r
    //| TwoPair h -> h
    | Pair hr -> hr
    | HighCard hr -> hr
    | _ -> failwith "Oh, no."

let bestHands hands =
    let parseHand s = (Hand.parseHand s),s
    let toPokerHand(h,s) = (toHandRank h),s
    let pokerHands = hands |> List.map(parseHand >> toPokerHand)
    
    let max =
        pokerHands
        |> List.maxBy(fun (ph,s) -> ph)
        |> fst
    pokerHands
    |> List.filter(fun (ph,s) -> ph = max)
    |> List.map(fun (ph,s) -> s)

//|> List.maxBy(fun (ph,s) -> ph)
//|> List.map (fun (ph,s) -> s)
bestHands ["2S 8H 6S 8D JH";"4S 5H 4C 8C 5C"]
