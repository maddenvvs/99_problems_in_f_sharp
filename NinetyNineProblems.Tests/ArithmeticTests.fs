module ArithmeticTests

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

[<Fact>]
let ``primeFactors returns list with dividers in ascending order`` () =
    Assert.True([3; 3; 5; 7] = primeFactors 315)

[<Fact>]
let ``primeFactors works well with large numbers`` () =
    Assert.True([3; 3; 239; 4649] = primeFactors 9999999)

[<Theory>]
[<InlineData(1)>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(-17)>]
[<InlineData(-1000)>]
let ``primeFactorsMult returns empty list for numbers less then 2`` n =
    Assert.True([] = primeFactorsMult n)

[<Theory>]
[<InlineData(3)>]
[<InlineData(5)>]
[<InlineData(7)>]
[<InlineData(11)>]
[<InlineData(7919)>]
[<InlineData(104_729)>]
[<InlineData(2_147_483_647)>]
let ``primeFactorsMult returns list with one number for prime numbes`` n =
    Assert.True([(n, 1)] = primeFactorsMult n)

[<Fact>]
let ``primeFactorsMult returns list with dividers in ascending order`` () =
    Assert.True([(3, 2); (5, 1); (7, 1)] = primeFactorsMult 315)

[<Fact>]
let ``primeFactorsMult works well with large numbers`` () =
    Assert.True([(3, 2); (239, 1); (4649, 1)] = primeFactorsMult 9999999)

[<Fact>]
let ``primesRange produces list of primes in given range`` () =
    Assert.True([2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31] =
        primesRange 1 32)

[<Fact>]
let ``primesRange produces list of primes for medium range`` () =
    Assert.Equal(1000, (primesRange 1 7920 |> List.length))

[<Theory>]
[<InlineData(2)>]
[<InlineData(1)>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(-97)>]
let ``goldbach produces empty list for numbers lower than 3`` n =
    Assert.True([] = goldbach n)

[<Theory>]
[<InlineData(3)>]
[<InlineData(5)>]
[<InlineData(21)>]
[<InlineData(2_147_483_647)>]
let ``goldbach produces empty list for odd positive numbers`` n =
    Assert.True([] = goldbach n)

[<Theory>]
[<InlineData(4)>]
[<InlineData(6)>]
[<InlineData(22)>]
[<InlineData(2_147_483_646)>]
let ``goldbach produces list with two numbers for even positive numbers greater than 2`` n =
    Assert.Equal(2, List.length (goldbach n))

[<Fact>]
let ``goldbach produces correct sum of two numbers`` () =
    Assert.True([5; 23] = goldbach 28)

[<Fact>]
let ``goldbachList produces the same output as in the initial task description`` () =
    Assert.True([[10; 3; 7]; [12; 5; 7]; [14; 3; 11];
                [16; 3; 13]; [18; 5; 13]; [20; 3; 17]] =
        goldbachList 9 20)

[<Fact>]
let ``goldbachListLimit produces the same output as in the initial task description`` () =
    Assert.True([[992; 73; 919]; [1382; 61; 1321];
                [1856; 67; 1789]; [1928; 61; 1867]] =
        goldbachListLimit 1 2000 50)

[<Theory>]
[<InlineData(36, 63, 9)>]
[<InlineData(1, 2, 1)>]
[<InlineData(500, 1000, 500)>]
[<InlineData(13, 27, 1)>]
[<InlineData(20536, 7826, 2)>]
let ``gcd finds greatest common divisor correctly`` a b g =
    Assert.Equal(g, gcd (a, b))

[<Theory>]
[<InlineData(1, 2)>]
[<InlineData(13, 27)>]
let ``coprime identifies coprimes correctly`` a b =
    Assert.True(coprime (a, b))

[<Theory>]
[<InlineData(1, 1)>]
[<InlineData(9, 6)>]
[<InlineData(74, 36)>]
[<InlineData(98, 42)>]
[<InlineData(10009, 10008)>]
[<InlineData(10090, 4032)>]
let ``phiNaive counts number of coprime numbers up to m`` m phi =
    Assert.Equal(phi, phiNaive m)

[<Theory>]
[<InlineData(1, 1)>]
[<InlineData(9, 6)>]
[<InlineData(74, 36)>]
[<InlineData(98, 42)>]
[<InlineData(10009, 10008)>]
[<InlineData(10090, 4032)>]
let ``phiImproved counts number of coprime numbers up to m`` m phi =
    Assert.Equal(phi, phiImproved m)

[<Fact>]
let ``phiImproved works faster than phiNaive`` () =
    Assert.True((comparePhis 10090) > 0L)
