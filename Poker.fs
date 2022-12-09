module Poker


open CardConverter
open Model

type Ranker<'R> = Ranker of (Card list -> Option<'R * Card list>)

let runR (Ranker f) state = f state

let returnR x =
    let inner state = (x,state) |> Some
    Ranker inner

let bindR f xR =
    let inner state =
        runR xR state |> Option.bind(fun (x,newState) -> runR (f x) newState)
    Ranker inner

let (>>=) xR f = bindR f xR

// type RankerBuilder() =
//     member this.Return(x) = returnR x
//     member this.ReturnFrom(xR) = xR
//     member this.Bind(xR,f) = bindR f xR
// let getR =
//     let inner cards = Some(cards,cards)
//     Ranker inner
// let putR newCards =
//     let inner _ = Some((),newCards)
//     Ranker inner
// let ranker = new RankerBuilder()
let mapR f r =
    let inner cards = runR r cards |> Option.map(fun (rs,cs) -> f rs,cs)
    Ranker inner

let (|>>) r f = mapR f r
let (.>>.) rR1 rR2 = rR1 >>= (fun r1 -> rR2 >>= (fun r2 -> returnR(r1,r2)))

let (>>.) a b =
    let inner cards =
        match runR a cards with
        | Some _ -> runR b cards
        | _ -> None
    Ranker inner

let (<|>) rR1 rR2 =
    let inner cards =
        let res1 = runR rR1 cards
        match res1 with
        | Some _ -> res1
        | _ -> runR rR2 cards
    Ranker inner

let chooseR list = list |> List.reduce (<|>)
let cardsToRanks = List.map Card.Rank >> List.sortDescending

let remainingRanksR =
    let ranker cards =
        cards
        |> cardsToRanks
        |> fun cs -> Some(cs,[])
    Ranker ranker

let remainingRanks = cardsToRanks
let highCardR = remainingRanksR |> mapR HandRank.HighCard

let ofKindRanksR n =
    let inner cards =
        cards
        |> List.groupBy Card.Rank
        |> List.sortByDescending fst
        |> List.tryFind(fun (r,cs) -> List.length cs = n)
        |> Option.map(fun (r,cs) -> 
               let remaining = cards |> List.except cs
               r,remaining)
    Ranker inner

let pairR = ofKindRanksR 2 .>>. remainingRanksR |>> HandRank.Pair
let twoPairR =
    ofKindRanksR 2 .>>. ofKindRanksR 2 .>>. remainingRanksR |>> HandRank.TwoPair
let treeOfKindR = ofKindRanksR 3 .>>. remainingRanksR |>> HandRank.ThreeOfKind
let fourOfKindR = ofKindRanksR 4 .>>. remainingRanksR |>> HandRank.FourOfKind
let fullHouseR = ofKindRanksR 3 .>>. ofKindRanksR 2 |>> HandRank.FullHouse

let straightRankR' transform =
    let distance a b = compare a b
    
    let inner cards =
        let ranks =
            cards
            |> transform
            |> cardsToRanks
        ranks
        |> List.pairwise
        |> List.forall(fun (one,another) -> distance one another = 1)
        |> function 
        | true -> (ranks |> List.max,[]) |> Some
        | false -> None
    Ranker inner

let straightLowR =
    let transform =
        List.map(fun c -> 
            if c.rank = Rank.HighAce then {c with rank = Rank.LowAce}
            else c)
    straightRankR' transform

let straightHighR = straightRankR' id
let straightRankR = straightHighR <|> straightLowR
let straightR = straightRankR |>> HandRank.Straight

let flushRanksR =
    let inner cards =
        cards
        |> List.map Card.Suit
        |> List.distinct
        |> List.length
        |> (=) 1
        |> function 
        | true -> 
            cards
            |> cardsToRanks
            |> fun rs -> Some(rs,[])
        | false -> None
    Ranker inner

let flushR = flushRanksR |>> HandRank.Flush
let straightFlushR = flushRanksR >>. straightRankR |>> HandRank.StraightFlush
let highestHandR =
    chooseR 
        [straightFlushR;fourOfKindR;fullHouseR;flushR;straightR;treeOfKindR;
         twoPairR;pairR;highCardR]

let bestHands hands =
    let parseHand s = (Hand.parseHand s),s
    
    let handRanks =
        hands
        |> List.map(parseHand
                    >> (fun ((Hand cards),s) -> 
                    match runR highestHandR cards with
                    | Some r -> fst r,s
                    | None -> failwith "fail -_-"))
    
    let max =
        handRanks
        |> List.map fst
        |> List.max
    
    handRanks
    |> List.filter(fun (handRank,s) -> handRank = max)
    |> List.map(fun (hr,s) -> s)
