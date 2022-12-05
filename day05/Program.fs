open Aochelper

let input = puzzleInput 5

type Instr = {
    amount: int;
    fromPile: int;
    toPile: int;
}

type Crate = char

type ParsedInput = {
    crates: List<List<Crate>>;
    instrs: Instr[];
}


//Parsing input
let getCrates (cratesInput: string[]) : List<List<Crate>> =
    let  pileToStringIndex (index: int) = index*4+1
    let piles: List<List<Crate>> =
        cratesInput[cratesInput.Length-1].Split " "
        |> Array.filter (fun s -> s <> "")
        |> Array.map (fun _ -> [])
        |> Array.toList

    cratesInput[0..cratesInput.Length-2]
    |> Array.rev
    |> Array.fold (fun (piles: Crate list list) (nextRow: string) ->
        piles
        |> List.indexed
        |> List.map (fun (index,pile: List<Crate>) ->
            if pileToStringIndex index < nextRow.Length
                && nextRow[pileToStringIndex index] <> ' '
            then
                nextRow[pileToStringIndex index]::pile
            else
                pile
        )
    ) piles

let getInstrs (instrsInput: string[]) : Instr[] =
    instrsInput
        |> Array.map (fun s ->
            s.Split " " |> fun ss ->
                { amount = int ss[1]
                ; fromPile = int ss[3] - 1
                ; toPile = int ss[5] - 1
                }
    )

let parsedInput: ParsedInput =
    let splitIndex = Array.findIndex (fun s -> s = "") input

    { crates = getCrates input[0..splitIndex-1]
    ; instrs = getInstrs input[splitIndex+1..input.Length-1]}


// Calculating solution
let topCrate (topCrate: Crate list) (pile: Crate list) =
    List.head pile :: topCrate

let moveCrates (rev: bool) (piles: Crate list list) (instr : Instr) =
    let movedCrates =
        match rev with
        | true  -> List.take instr.amount piles[instr.fromPile] |> List.rev // For part 1
        | false -> List.take instr.amount piles[instr.fromPile] // For part 2

    let newFromPile = List.skip instr.amount piles[instr.fromPile]
    let newToPile = movedCrates @ piles[instr.toPile]

    piles
        |> List.updateAt instr.fromPile newFromPile
        |> List.updateAt instr.toPile newToPile

//part1
Array.fold (moveCrates true) parsedInput.crates parsedInput.instrs
|> List.fold topCrate []
|> List.rev
|> List.toArray
|> System.String
|> submitAnswer 5 2

//part2
Array.fold (moveCrates false) parsedInput.crates parsedInput.instrs
|> List.fold topCrate []
|> List.rev
|> List.toArray
|> System.String
|> submitAnswer 5 2