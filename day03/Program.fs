open Aochelper

let input = puzzleInput 3

let compartments(rucksack: string) =
    let middle = rucksack.Length/2
    (rucksack[0..middle-1], rucksack[middle..rucksack.Length])

let shared (comp1: string, comp2: string) =
    String.filter comp2.Contains comp1

let getPriority (items: string) =
    let c: char = Seq.toList >> List.item 0 <| items
    let num = int c - int 'A'
    if num > 26 then
        num - 31
    else
        num + 27

//Part 1
Array.map (compartments >> shared >> getPriority) input
    |> Array.sum
    |> submitAnswerInt 3 1

let shared3 (elves: string[]) =
    shared (elves[0], elves[1]) |> curry shared elves[2]

//Part 2
Array.chunkBySize 3 input
    |> Array.map (shared3 >> getPriority)
    |> Array.sum
    |> submitAnswerInt 3 2
