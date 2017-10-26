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
            {2..upper} |> Seq.forall (fun dv -> n % dv <> 0)
