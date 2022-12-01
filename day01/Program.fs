
open Aochelper

let input = puzzleInput 1

let folder (cal: int, elfCalories) (nextInput: string) =
    if nextInput.Equals "" then
        (0, cal::elfCalories)
    else
        (cal + int(nextInput), elfCalories)

let elfCalories = snd <| Array.fold folder (0,[]) input

List.max elfCalories
    |> string
    |> submitAnswer 1 1

List.sortDescending elfCalories
    |> List.take 3
    |> List.sum
    |> submitAnswerInt 1 2