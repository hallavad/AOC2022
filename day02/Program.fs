open Aochelper

let input = puzzleInput 2

let folder1 (score) (nextInput: string) =
    let choices = nextInput.Split(' ')

    score
    + match (choices[0], choices[1]) with
      | "A", "X" -> 3 + 1
      | "A", "Y" -> 6 + 2
      | "A", "Z" -> 0 + 3
      | "B", "X" -> 0 + 1
      | "B", "Y" -> 3 + 2
      | "B", "Z" -> 6 + 3
      | "C", "X" -> 6 + 1
      | "C", "Y" -> 0 + 2
      | "C", "Z" -> 3 + 3

let folder2 (score) (nextInput: string) =
    let choices = nextInput.Split(' ')

    score
    + match (choices[0], choices[1]) with
      | "A", "X" -> 0 + 3
      | "A", "Y" -> 3 + 1
      | "A", "Z" -> 6 + 2
      | "B", "X" -> 0 + 1
      | "B", "Y" -> 3 + 2
      | "B", "Z" -> 6 + 3
      | "C", "X" -> 0 + 2
      | "C", "Y" -> 3 + 3
      | "C", "Z" -> 6 + 1

Array.fold folder1 0 input |> submitAnswerInt 2 1

Array.fold folder2 0 input |> submitAnswerInt 2 2
