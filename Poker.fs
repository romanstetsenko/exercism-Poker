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
    let inner = List.map fst >> List.sortDescending
    inner

let (|HighCard|_|)(Hand cards) =
    remainingRanks cards
    |> HandRank.HighCard
    |> Some

let findOfKindRanks n =
    let inner(cards: Card list) =
        cards
        |> List.groupBy fst
        |> List.sortByDescending fst
        |> List.tryFind(fun (r,cs) -> List.length cs = n)
        |> Option.map(fun (r,cs) -> 
               let remaining =
                   cards
                   |> List.except cs
                   |> remainingRanks
               r,remaining)
    inner

let (|Pair|_|)(Hand hand) = findOfKindRanks 2 hand |> Option.map HandRank.Pair
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
let (|TreeOfKind|_|)(Hand hand) =
    findOfKindRanks 3 hand |> Option.map HandRank.ThreeOfKind
let (|FourOfKind|_|)(Hand hand) =
    findOfKindRanks 4 hand |> Option.map HandRank.FourOfKind

let toHandRank hand =
    match hand with
    //     | StraightFlush r
    | FourOfKind hr -> hr 
    //     | FullHouse r
    //     | Flush r
    //| Straight r
    | TreeOfKind hr -> hr
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
