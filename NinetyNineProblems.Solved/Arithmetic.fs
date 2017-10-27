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

    (*
        2.03
        Construct a list containing the prime factors and their multiplicity.
    *)
    let primeFactorsMult =
        let rec aux counter res = function
            | [] -> res
            | [x] -> (x, counter + 1) :: res
            | x :: (y :: _ as xs) ->
                if x = y
                then aux (counter + 1) res xs
                else aux 0 ((x, counter + 1) :: res) xs
        primeFactors >> aux 0 [] >> List.rev

    (*
        2.04
        A list of prime numbers.
    *)
    let primesRange m n =
        let rec aux acc = function
            | t when t > n -> acc
            | t ->
                if isPrime t
                then aux (t :: acc) (t + 1)
                else aux acc (t + 1)
        List.rev (aux [] m)

