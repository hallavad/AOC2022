open Aochelper

let input = puzzleInput 4

type assignment = int * int

let assignments (nextInput: string): assignment * assignment =
    nextInput.Split(',')
    |> Array.map (
        fun (elfPair: string) -> elfPair.Split('-')
        >> Array.map (int)
        >> Array.toList >> listToTuple)
    |> (Array.toList >> listToTuple)

//Part 1
let oneIsSubset ((s1,e1),(s2,e2)) = if (s1 <= s2 && e1 >= e2) || (s2 <= s1 && e2 >= e1) then 1 else 0

Array.map assignments input
    |> Array.sumBy oneIsSubset
    |> submitAnswerInt 4 1

//Part 2
let isOverlapping ((s1,e1),(s2,e2)) = if (s1 <= e2 && e1 >= s2) || (s2 <= e1 && e2 >= s1) then 1 else 0

Array.map assignments input
    |> Array.sumBy (isOverlapping)
    |> submitAnswerInt 4 2