namespace NinetyNineProblems.Solved

module Arithmetic =

    (*
        2.01
        Determine whether a given integer number is prime.
    *)
    let isPrime = function
        | n when n < 2 -> false
        | 2 -> true
        | n when n % 2 = 0 -> false
        | n ->
            let upper = (double n) |> sqrt |> ceil |> int
            {3..2..upper} |> Seq.forall (fun dv -> n % dv <> 0)

    (*
        2.02
        Determine the prime factors of a given positive integer.
    *)
    let primeFactors n =
        let rec aux d acc = function
            | k when k < 2 -> acc
            | k when k % 2 = 0 -> aux d (2 :: acc) (k / 2)
            | k ->
                if k % d = 0
                then aux d (d :: acc) (k / d)
                else aux (d + 2) acc k
        List.rev (aux 3 [] n)
