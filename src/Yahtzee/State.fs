module Yahtzee.State

//strategies 
module Strategies = 
    type Strategy = seq<int> -> int

    let highestRepeated dice minRepeats = 
        let repeats = dice |> List.countBy id |> List.filter (fun (_,n) -> n >= minRepeats) |> List.map fst 
        match repeats with | [] -> 0 | _ -> List.max repeats

    let ofAKind n dice = 
        n * highestRepeated dice n

    let sumOfSingle (selected : int) (dice : #seq<int>) = 
        dice |> Seq.filter ((=) selected) |> Seq.sum

    let chance : Strategy = Seq.sum
    let ones : Strategy = sumOfSingle 1
    let twos : Strategy = sumOfSingle 2
    let threes : Strategy = sumOfSingle 3
    let fours : Strategy = sumOfSingle 4
    let fives : Strategy = sumOfSingle 5
    let sixes : Strategy = sumOfSingle 6
    let pair = ofAKind 2
    let threeOfAKind = ofAKind 3
    let fourOfAKind = ofAKind 4
    let straight target score dice = 
        if List.sort dice = target then score else 0
    let smallStraight = straight [1;2;3;4;5] 15
    let largeStraight = straight [2;3;4;5;6] 20
    let yahtzee dice = 
        if Seq.length dice = 5 && Seq.length (Seq.distinct dice) = 1 then 50 else 0
                

    