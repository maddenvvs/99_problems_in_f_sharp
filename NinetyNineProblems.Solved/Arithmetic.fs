namespace NinetyNineProblems.Solved

open System.Diagnostics

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

    (*
        2.05
        Goldbach's conjecture.
    *)
    let goldbach = function
        | n when (n < 3) || (n % 2 = 1) -> []
        | n when n = 4 -> [2; 2]
        | n ->
            let rec aux = function
                | t when t > n -> []
                | t ->
                    if isPrime t && isPrime (n - t)
                    then [t; n-t]
                    else aux (t + 2)
            aux 3

    (*
        2.06
        Given a range of integers by its lower and upper limit,
        print a list of all even numbers and their Goldbach composition.
    *)
    let goldbachList m n =
        let rec aux acc t =
            if t > n
            then acc
            else match goldbach t with
                 | [] -> aux acc (t + 2)
                 | l -> aux ((t :: l) :: acc) (t + 2)
        List.rev (aux [] (if m % 2 = 0 then m else (m + 1)))

    (*
        2.06 (continue)
    *)
    let goldbachListLimit m n l =
        goldbachList m n
        |> List.filter (fun [_; x; y] -> x > l && y > l)

    (*
        2.07
        Greatest common divisor
    *)
    let rec gcd = function
        | (m, 0) -> m
        | (m, n) -> gcd (n, m % n)

    (*
        2.08
        Determine whether two positive integer numbers are coprime.
    *)
    let coprime = gcd >> (=) 1

    (*
        2.09
        Euler's totient function phi(m) is defined as the number
        of positive integers r (1 <= r < m) that are coprime to m.
    *)
    let phiNaive m =
        let rec aux acc = function
            | r when r >= m -> acc
            | r ->
                if coprime (m, r)
                then aux (acc + 1) (r + 1)
                else aux acc (r + 1)
        aux 1 2

    (*
        2.10
        Calculate Euler's totient function phi(m).
    *)
    let phiImproved =
        primeFactorsMult
        >> List.fold (fun v (p, m) -> v * (p - 1) * (pown p (m - 1))) 1

    (*
        2.11
        Compare the two methods of calculating Euler's totient function.
    *)
    let timeOf f a =
        let sw = Stopwatch.StartNew()
        f a |> ignore
        sw.ElapsedMilliseconds

    let comparePhis m =
        let phiNaiveTime = timeOf phiNaive m
        let phiImprovedTime = timeOf phiImproved m
        phiNaiveTime - phiImprovedTime
