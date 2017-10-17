module Lists_Tests

open System
open Xunit
open NinetyNineProblems.Solved.Lists

[<Fact>]
let ``last on empty list returns None`` () =
    Assert.Equal(None, last [])

[<Fact>]
let ``last on list with one element returns this element`` () =
    Assert.Equal(Some 1, last [1])

[<Fact>]
let ``last on list with two elements returns the last one element`` () =
    Assert.Equal(Some 2, last [1; 2])

[<Fact>]
let ``last_two on empty list returns None`` () =
    Assert.Equal(None, last_two [])

[<Fact>]
let ``last_two on list with one element returns None`` () =
    Assert.Equal(None, last_two [1])

[<Fact>]
let ``last_two on list with two elements returns these elements`` () =
    Assert.Equal(Some (1, 2), last_two [1; 2])

[<Fact>]
let ``last_two on list with three elements returns last two elements`` () =
    Assert.Equal(Some (2, 3), last_two [1; 2; 3])
