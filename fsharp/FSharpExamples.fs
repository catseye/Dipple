// fsharpi --quiet --exec FSharpExamples.fs

module FSharpExamples

//-----------------------------
// Recursive function, match
//-----------------------------

let rec factorial = function
    | 0 -> 1
    | 1 -> 1
    | n -> n * factorial (n - 1)

let atoi s =
    match System.Int32.TryParse(s) with
    | (true, value) -> Some value
    | _             -> None

let printFactorialOfString s =
    match atoi s with
    | Some value -> printfn "%d" (factorial value)
    | None       -> printfn "%s?" s

let _ = printFactorialOfString "5"
let _ = printFactorialOfString "five"

//-----------------------------
// Tuple, System.String
//-----------------------------

let _ =
    let
        message = System.String.Join(",", ["Moe";"Larry";"Curly"])
    in
        printfn "%s" message

//-----------------------------
// Sets
//-----------------------------

let rollCall =
    let set1 = Set.ofList ["Moe";"Moe";"John"]
    let set2 = Set.ofList ["Moe";"Larry";"Curly"]
    let set3 = (Set.intersect set1 set2).Add("Perry")
    Set.map (fun x -> x + "!") set3

let _ = printfn "%A" rollCall

//-----------------------------
// Maps
//-----------------------------

let mapThing who =
    let map1 = Map.add "Moe" 21 Map.empty
    let map2 = map1.Add("Larry", 4).Add("Curly", 8)
    if (Map.containsKey who map2) then map2.[who] else 0

let _ = printfn "%A" (mapThing "Larry")

//-----------------------------
// Records
//-----------------------------

type record = {
    Name : string;
    Age : int;
}

let charlie = { Name = "Charlie"; Age = 8 }

let isCharlieOrUnderage = function
    | { Name = "Charlie" } -> true
    | person when person.Age < 18 -> true
    | _ -> false

let happyBirthday person = { person with Age = person.Age + 1 }

let _ = printfn "%A" (happyBirthday charlie)

//-----------------------------
// Console, main()
//-----------------------------

open System

let main() =
    printfn "%s" "Press any key to continue"
    Console.ReadKey(true) |> ignore

main()
