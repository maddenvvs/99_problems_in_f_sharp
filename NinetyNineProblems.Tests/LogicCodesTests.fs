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
