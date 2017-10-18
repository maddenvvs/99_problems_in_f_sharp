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

[<Fact>]
let ``Lists.element_at returns None if position is lower than 1`` () =
    Assert.Equal(None, Lists.element_at 0 [1; 2; 3])

[<Fact>]
let ``Lists.element_at returns None if list is empty`` () =
    Assert.Equal(None, Lists.element_at 1 [])

[<Fact>]
let ``Lists.element_at returns None if index is greater than list length`` () =
    Assert.Equal(None, Lists.element_at 3 [1; 2])

[<Fact>]
let ``Lists.element_at returns element if index is lower than list length`` () =
    Assert.Equal(Some 2, Lists.element_at 2 [1; 2; 3])
