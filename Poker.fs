module Poker

open Model
open CardConverter

let rnk = fst

let remainingRanks =
    let inner = List.map rnk >> List.sortDescending
    inner

let (|HighCard|_|)(Hand cards) =
    remainingRanks cards
    |> HandRank.HighCard
    |> Some

let findOfKindRanks n =
    let inner(cards: Card list) =
        cards
        |> List.groupBy rnk
        |> List.sortByDescending rnk
        |> List.tryFind(fun (r,cs) -> List.length cs = n)
        |> Option.map(fun (r,cs) -> 
               let remaining = cards |> List.except cs
               r,remaining)
    inner

let (|Pair|_|)(Hand hand) =
    findOfKindRanks 2 hand 
    |> Option.map(fun (p,rs) -> HandRank.Pair(p,rs |> remainingRanks))
let (|TwoPair|_|)(Hand hand) =
    findOfKindRanks 2 hand 
    |> Option.bind
           (fun (p1,remainings) -> 
           findOfKindRanks 2 remainings 
           |> Option.map
                  (fun (p2,rs) -> HandRank.TwoPair(p1,p2,rs |> remainingRanks)))
let (|TreeOfKind|_|)(Hand hand) =
    findOfKindRanks 3 hand 
    |> Option.map(fun (p,rs) -> HandRank.ThreeOfKind(p,rs |> remainingRanks))
let (|FourOfKind|_|)(Hand hand) =
    findOfKindRanks 4 hand 
    |> Option.map(fun (p,rs) -> HandRank.FourOfKind(p,rs |> remainingRanks))
let (|FullHouse|_|)(Hand hand) =
    findOfKindRanks 3 hand 
    |> Option.bind
           (fun (p1,remainings) -> 
           findOfKindRanks 2 remainings 
           |> Option.map(fun (p2,rs) -> HandRank.FullHouse(p1,p2)))

let toHandRank hand =
    match hand with
    //     | StraightFlush r
    | FourOfKind hr -> hr
    | FullHouse hr -> hr
    //     | Flush r
    //| Straight r
    | TreeOfKind hr -> hr
    | TwoPair h -> h
    | Pair hr -> hr
    | HighCard hr -> hr
    | _ -> failwith "Oh, no."

let bestHands hands =
    let parseHand s = (Hand.parseHand s),s
    let toPokerHand(h,s) = (toHandRank h),s
    let handRanks = hands |> List.map(parseHand >> toPokerHand)
    
    let max =
        handRanks
        |> List.maxBy rnk
        |> rnk
    handRanks
    |> List.filter(fun (hr,s) -> hr = max)
    |> List.map(fun (hr,s) -> s)

//|> List.maxBy(fun (ph,s) -> ph)
//|> List.map (fun (ph,s) -> s)
bestHands ["2S 8H 6S 8D JH";"4S 5H 4C 8C 5C"]
