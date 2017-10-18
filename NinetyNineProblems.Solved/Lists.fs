namespace NinetyNineProblems.Solved

module Lists =

    let rec last = function
        | [] -> None
        | [x] -> Some x
        | _ :: xs -> last xs

    let rec last_two = function
        | [] | [_] -> None
        | [a; b] -> Some (a, b)
        | _ :: xs -> last_two xs

    let rec element_at k lst =
        match k, lst with
        | k, _ when k < 1 -> None
        | _, [] -> None
        | 1, x :: _ -> Some x
        | k, _ :: xs -> element_at (k-1) xs
