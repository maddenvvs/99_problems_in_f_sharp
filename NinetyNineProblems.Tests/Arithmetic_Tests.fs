module Arithmetic_Tests

open System
open Xunit
open NinetyNineProblems.Solved.Arithmetic

[<Fact>]
let ``isPrime treat 2 as prime`` () =
    Assert.Equal(true, isPrime 2)

[<Fact>]
let ``isPrime treat 1 as non-prime`` () =
    Assert.Equal(false, isPrime 1)

[<Theory>]
[<InlineData(3)>]
[<InlineData(5)>]
[<InlineData(7)>]
[<InlineData(11)>]
[<InlineData(7919)>]
[<InlineData(104_729)>]
[<InlineData(2_147_483_647)>]
let ``isPrime identifies primes correctly`` n =
    Assert.Equal(true, isPrime n)

[<Theory>]
[<InlineData(4)>]
[<InlineData(10)>]
[<InlineData(14)>]
[<InlineData(99)>]
[<InlineData(7920)>]
[<InlineData(104_731)>]
let ``isPrime identifies non-primes correctly`` n =
    Assert.Equal(false, isPrime n)

[<Theory>]
[<InlineData(-1)>]
[<InlineData(-2)>]
[<InlineData(-200_000)>]
let ``isPrime identifies negative numbers as non-primes`` n =
    Assert.Equal(false, isPrime n)

[<Theory>]
[<InlineData(1)>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(-17)>]
[<InlineData(-1000)>]
let ``primeFactors returns empty list for numbers less then 2`` n =
    Assert.True([] = primeFactors n)

[<Theory>]
[<InlineData(3)>]
[<InlineData(5)>]
[<InlineData(7)>]
[<InlineData(11)>]
[<InlineData(7919)>]
[<InlineData(104_729)>]
[<InlineData(2_147_483_647)>]
let ``primeFactors returns list with one number for prime numbes`` n =
    Assert.True([n] = primeFactors n)

let ``primeFactors returns list with dividers in ascending order`` () =
    Assert.True([3; 3; 5; 7] = primeFactors 315)

let ``primeFactors works well with large numbers`` () =
    Assert.True([3; 3; 239; 4649] = primeFactors 9999999)
