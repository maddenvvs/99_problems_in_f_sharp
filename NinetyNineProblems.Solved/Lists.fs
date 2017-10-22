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

    let encode lst =
        let rec aux counter res = function
            | [] -> res
            | [x] -> (counter + 1, x) :: res
            | x :: (y :: _ as xs) ->
                if x = y
                then aux (counter + 1) res xs
                else aux 0 ((counter + 1, x) :: res) xs
        List.rev (aux 0 [] lst)

    // This is also solution for problem 13.
    type 'a rlel =
        | One of 'a
        | Many of int * 'a
    let encode_modified lst =
        let create counter x =
            if counter = 0 then One x else Many (counter + 1, x)
        let rec aux counter res = function
            | [] -> res
            | [x] -> create counter x :: res
            | x :: (y :: _ as xs) ->
                if x = y
                then aux (counter + 1) res xs
                else aux 0 (create counter x :: res) xs
        List.rev (aux 0 [] lst)

    let decode_modified lst =
        let rec dupl times x acc =
            if times = 0 then acc else dupl (times-1) x (x :: acc)
        let rec aux acc = function
            | [] -> acc
            | One x :: xs -> aux (x :: acc) xs
            | Many (c, x) :: xs -> aux (dupl c x acc) xs
        aux [] (List.rev lst)

    (*
        Both duplicate and nduplicate reverse initial list
        to use better memory allocation pattern and tail-recursivity.
        decode_modified uses the same pattern because of the same reasons.
    *)
    let duplicate lst =
        let rec aux acc = function
            | [] -> acc
            | x :: xs -> aux (x :: (x :: acc)) xs
        aux [] (List.rev lst)

    let nduplicate count lst =
        let rec dupl times x acc =
            if times = 0u then acc else dupl (times-1u) x (x :: acc)
        let rec aux acc = function
            | [] -> acc
            | x :: xs -> aux (dupl count x acc) xs
        aux [] (List.rev lst)

    let drop k lst =
        let rec aux pos acc = function
            | [] -> acc
            | x :: xs ->
                if pos = k
                then aux 1u acc xs
                else aux (pos + 1u) (x :: acc) xs
        List.rev (aux 1u [] lst)

    let split k lst =
        let rec aux pos l = function
            | [] -> List.rev l, []
            | x :: xs as r ->
                if pos < k
                then aux (pos + 1u) (x :: l) xs
                else List.rev l, r
        aux 0u [] lst

    let slice l r lst =
        let rec aux i acc = function
            | [] -> List.rev acc
            | x :: xs ->
                match l <= i, i <= r with
                | true, true -> aux (i + 1u) (x :: acc) xs
                | false, _ -> aux (i + 1u) acc xs
                | _, false -> List.rev acc
        aux 1u [] lst

    let rotate n lst =
        let len = List.length lst
        match len with
        | 0 -> lst
        | len ->
            let n = (n % len + len) % len
            let l, r = split (uint32 n) lst
            r @ l
