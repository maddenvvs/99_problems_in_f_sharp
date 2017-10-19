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
            | x :: xs -> aux (x :: rev) xs
        aux [] lst

    let is_palindrome lst = lst = List.rev lst

    type 'a elem =
        | Flat of 'a
        | Nested of 'a elem list
    let flatten lst =
        let rec aux flat = function
            | [] -> flat
            | Flat x :: xs -> aux (x :: flat) xs
            | Nested x :: xs -> aux (aux flat x) xs
        List.rev (aux [] lst)

    (*
        Other possible solution:

        let rec compress = function
            | x :: (y :: _ as xs) -> if x = y then compress xs else x :: compress xs
            | l -> l

        It looks clear but doesn't have tail recursion.
    *)
    let compress lst =
        let rec aux acc = function
            | [] -> acc
            | [x] -> x :: acc
            | x :: (y :: _ as xs) ->
                if x = y
                then aux acc xs
                else aux (x :: acc) xs
        List.rev (aux [] lst)

    let pack lst =
        let rec aux tmp res = function
            | [] -> res
            | [x] -> (x :: tmp) :: res
            | x :: (y :: _ as xs) ->
                if x = y
                then aux (x :: tmp) res xs
                else aux [] ((x :: tmp) :: res) xs
        List.rev (aux [] [] lst)
