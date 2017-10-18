module Lists_Tests

open System
open Xunit
open NinetyNineProblems.Solved

[<Fact>]
let ``Lists.last on empty list returns None`` () =
    Assert.Equal(None, Lists.last [])

[<Fact>]
let ``Lists.last on list with one element returns this element`` () =
    Assert.Equal(Some 1, Lists.last [1])

[<Fact>]
let ``Lists.last on list with two elements returns the last one element`` () =
    Assert.Equal(Some 2, Lists.last [1; 2])

[<Fact>]
let ``Lists.last_two on empty list returns None`` () =
    Assert.Equal(None, Lists.last_two [])

[<Fact>]
let ``Lists.last_two on list with one element returns None`` () =
    Assert.Equal(None, Lists.last_two [1])

[<Fact>]
let ``Lists.last_two on list with two elements returns these elements`` () =
    Assert.Equal(Some (1, 2), Lists.last_two [1; 2])

[<Fact>]
let ``Lists.last_two on list with three elements returns last two elements`` () =
    Assert.Equal(Some (2, 3), Lists.last_two [1; 2; 3])
