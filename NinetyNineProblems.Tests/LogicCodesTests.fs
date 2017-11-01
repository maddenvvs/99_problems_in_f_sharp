module LogicCodesTests

open System
open Xunit
open NinetyNineProblems.Solved.LogicCodes

[<Theory>]
[<InlineData(false, false, true)>]
[<InlineData(false, true, true)>]
[<InlineData(true, false, false)>]
[<InlineData(true, true, true)>]
let ``Implication works as expected`` a b r =
    let varFunc v = if v = "a" then a else b

    Assert.Equal(r, evalBoolExpr varFunc (Impl (Var "a", Var "b")))

[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(-2)>]
[<InlineData(-10000)>]
let ``gray returns empty list for n less than 1`` n =
    Assert.True([] = gray n)

[<Fact>]
let ``gray returns correct codes for n = 1`` () =
    Assert.True(["0"; "1"] = gray 1)

[<Fact>]
let ``gray returns correct codes for n = 2`` () =
    Assert.True(["00"; "01"; "11"; "10"] = gray 2)

[<Fact>]
let ``gray returns correct codes for n = 3`` () =
    Assert.True(["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"] =
        gray 3)
