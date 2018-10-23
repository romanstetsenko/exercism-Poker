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

type Card = Rank * Suit

type Hand = Hand of Card list

type RemainingRanks = Rank list

type HandRank =
    | HighCard of RemainingRanks
    | Pair of Rank * RemainingRanks
    | TwoPair of Rank * Rank * RemainingRanks
    | ThreeOfKind of Rank * RemainingRanks
    | Straight of Rank
    | Flush of Rank
    | FullHouse of Rank
    | FourOfKind of Rank * RemainingRanks
    | StraightFlush of Rank

type PokerHand = PokerHand of HandRank * string