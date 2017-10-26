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