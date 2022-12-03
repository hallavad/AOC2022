module Aochelper

open FsHttp
open System.IO
open System.Text.RegularExpressions

let baseURL: string = "https://adventofcode.com/2022/"
let inputBasePath: string = "input/"

let getToken: string =
    File.ReadAllLines("token.env")[0]

//
// Functions for submitting answers
//

let sendAnswer (day: int, p: int) (answer: string) =
    http {
        POST (baseURL + "day/" + string(day) + "/answer")
        Cookie "session" getToken
        body
        formUrlEncoded [
            "level", string(p)
            "answer", answer
        ]
    }
    |> Request.send
    |> Response.toString None



let printAnswerResponse (resp: string) =
    let mainIndex = resp.IndexOf "<main>" + 7
    let mainLength = (resp.IndexOf "</main>") - mainIndex
    let mainNode: string = resp.Substring (mainIndex, mainLength)

    let text = Regex.Replace(mainNode, @"<.*?>", "")
    printfn "%s" text

let submitAnswer (day: int) (p: int) (ans: string) =
    let a = sendAnswer (day, p) ans
    a |> printAnswerResponse
    printfn "%s" ans

let submitAnswerInt (day: int) (p: int) (ans: int) = submitAnswer day p <| string(ans)

//
// Functions for getting input
//


let requestInput (day: int): string =
    http {
        GET (baseURL + "day/" + string(day) + "/input")
        Cookie "session" getToken
    }
    |> Request.send
    |> Response.toString None

let saveInput (day: int) (input: string) =
    use sw = File.CreateText (inputBasePath + string(day))
    sw.Write input

let readInput (day: int) =
    File.ReadAllLines (inputBasePath + string(day))

let puzzleInput (day : int) =
    if not <| File.Exists (inputBasePath + string(day)) then
        let input = requestInput day
        saveInput day input
    readInput day


// helpful functions
let curry f a b = f (a, b)
let uncurry f (a,b) = f a b