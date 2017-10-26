module Arithmetic_Tests

open System
open Xunit
open NinetyNineProblems.Solved.Arithmetic

[<Fact>]
let ``is_prime treat 2 as prime`` () =
    Assert.Equal(true, is_prime 2)

[<Fact>]
let ``is_prime treat 1 as non-prime`` () =
    Assert.Equal(false, is_prime 1)

[<Theory>]
[<InlineData(3)>]
[<InlineData(5)>]
[<InlineData(7)>]
[<InlineData(11)>]
[<InlineData(7919)>]
[<InlineData(104729)>]
[<InlineData(2_147_483_647)>]
[<InlineData(-2_147_483_647)>]
let ``is_prime identifies primes correctly`` n =
    Assert.Equal(true, is_prime n)

[<Theory>]
[<InlineData(4)>]
[<InlineData(10)>]
[<InlineData(14)>]
[<InlineData(99)>]
[<InlineData(7920)>]
[<InlineData(104731)>]
[<InlineData(-104731)>]
let ``is_prime identifies non-primes correctly`` n =
    Assert.Equal(false, is_prime n)
