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

    (*
        This implementation is tail-recursive
        and optimized by F# compiler to produce
        constant stack size solution.

        Other possible solution:

        let rec len = function
            | [] -> 0
            | _ :: xs -> 1 + len xs

        But this solution throws StackOverflowException
        on huge lists.
    *)
    let len lst =
        let rec aux n = function
            | [] -> n
            | _ :: xs -> aux (n+1) xs
        aux 0 lst

    let reverse lst =
        let rec aux rev = function
            | [] -> rev
            | x :: xs -> aux ([x] @ rev) xs
        aux [] lst

    let is_palindrome lst = lst = List.rev lst
