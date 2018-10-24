module Model

(* exercism.exe submit .\Model.fs .\CardConverter.fs .\Poker.fs .\Poker.fsproj *)
type Rank =
    | LowAce
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | HighAce

type Suit =
    | Spades
    | Clubs
    | Diamands
    | Hearts

type Card = { rank: Rank; suit: Suit} with
    static member Rank {rank=rank} = rank
    static member Suit {suit=suit} = suit

type Hand = Hand of Card list

type HandRank =
    | HighCard of Rank list
    | Pair of Rank * Rank list
    | TwoPair of (Rank * Rank) * Rank list
    | ThreeOfKind of Rank * Rank list
    | Straight of Rank
    | Flush of Rank list 
    | FullHouse of Rank * Rank
    | FourOfKind of Rank * Rank list
    | StraightFlush of Rank
