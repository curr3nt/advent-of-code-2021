// solution #1 - using 2 FOR cycles

open System.Collections.Generic

let printList list = Seq.fold (fun s o -> sprintf "%s%O," s o) "" list

let mutable fishes1 = List<int>([3;4;3;1;2])

let iterations = [1 .. 80]

fishes1 |> printList |> printfn "Initial state:\t%s"

for i in iterations do
    let newState = List<int>()
    let newBorn = List<int>()
    for f in fishes1 do
        if f = 0 
        then 
            newState.Add(6)
            newBorn.Add(8)
        else
            newState.Add(f - 1)
    newState.AddRange(newBorn)
    fishes1 <- newState
    fishes1 |> printList |> printfn "After\t%i day:\t%O" i
    printfn "%i" (Seq.length fishes1)

// solution #2 - using recursion and caching

open System.Collections.Generic

let rec exhaust fish i children =
    if i = 0
    then children
    else
        if fish = 0
        then exhaust 6 (i - 1) ((8, i - 1) :: children)
        else exhaust (fish - 1) (i - 1) children

let cache = Dictionary<int * int, int64>()

let rec count fishes (totalCount: int64) =
    match fishes with
    | (fish, i) :: rest -> 
        let total =
            if cache.ContainsKey(fish, i)
            then 
                cache.[fish, i]
            else
                let children = exhaust fish i []
                let total = count children (Seq.length children |> int64)
                cache.Add((fish, i), total)
                total
        count rest (totalCount + total)
    | [] -> totalCount

let fishes = [5;1;1;5;4;2;1;2;1;2;2;1;1;1;4;2;2;4;1;1;1;1;1;4;1;1;1;1;1;5;3;1;4;1;1;1;1;1;4;1;5;1;1;1;4;1;2;2;3;1;5;1;1;5;1;1;5;4;1;1;1;4;3;1;1;1;3;1;5;5;1;1;1;1;5;3;2;1;2;3;1;5;1;1;4;1;1;2;1;5;1;1;1;1;5;4;5;1;3;1;3;3;5;5;1;3;1;5;3;1;1;4;2;3;3;1;2;4;1;1;1;1;1;1;1;2;1;1;4;1;3;2;5;2;1;1;1;4;2;1;1;1;4;2;4;1;1;1;1;4;1;3;5;5;1;2;1;3;1;1;4;1;1;1;1;2;1;1;4;2;3;1;1;1;1;1;1;1;4;5;1;1;3;1;1;2;1;1;1;5;1;1;1;1;1;3;2;1;2;4;5;1;5;4;1;1;3;1;1;5;5;1;3;1;1;1;1;4;4;2;1;2;1;1;5;1;1;4;5;1;1;1;1;1;1;1;1;1;1;3;1;1;1;1;1;4;2;1;1;1;2;5;1;4;1;1;1;4;1;1;5;4;4;3;1;1;4;5;1;1;3;5;3;1;2;5;3;4;1;3;5;4;1;3;1;5;1;4;1;1;4;2;1;1;1;3;2;1;1;4]
let initial = fishes |> List.map (fun i -> i, 256)
let total = count initial (Seq.length initial |> int64)
printfn "total: %i" total