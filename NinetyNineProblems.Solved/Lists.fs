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
