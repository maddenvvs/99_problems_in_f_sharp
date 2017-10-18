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
let ``Lists.last on huge list returns the last element`` () =
    Assert.Equal(Some 1000000, Lists.last [1..1000000])

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
let ``Lists.last_two on huge list returns last two elements`` () =
    Assert.Equal(Some (999999, 1000000), Lists.last_two [1..1000000])

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

[<Fact>]
let ``Lists.element_at returns element if list is huge`` () =
    Assert.Equal(Some 999999, Lists.element_at 999999 [1..1000000])

[<Fact>]
let ``Lists.len returns 0 if list is empty`` () =
    Assert.Equal(0, Lists.len [])

[<Fact>]
let ``Lists.len returns 3 if list contains three elements`` () =
    Assert.Equal(3, Lists.len [1; 2; 3])

[<Fact>]
let ``Lists.len returns correct result if list is large`` () =
    Assert.Equal(1000000, Lists.len [1..1000000])

[<Fact>]
let ``Lists.reverse returns empty list when initial list is empty`` () =
    Assert.True([] = Lists.reverse [])

[<Fact>]
let ``Lists.reverse returns list with one element when initial list contains one element`` () =
    Assert.True([1] = Lists.reverse [1])

[<Fact>]
let ``Lists.reverse reverses list with four elements`` () =
    Assert.True([4; 2; 3; 1] = Lists.reverse [1; 3; 2; 4])

[<Fact>]
let ``Lists.reverse reverses list when list is huge`` () =
    Assert.True([1000000..-1..1] = Lists.reverse [1..1000000])

[<Fact>]
let ``Lists.is_palindrome returns true for empty list`` () =
    Assert.True(Lists.is_palindrome [])

[<Fact>]
let ``Lists.is_palindrome returns true for list with one element`` () =
    Assert.True(Lists.is_palindrome [1])

[<Fact>]
let ``Lists.is_palindrome returns true for list with even number of elements`` () =
    Assert.True(Lists.is_palindrome [1; 1])

[<Fact>]
let ``Lists.is_palindrome returns false for list with even number of elements`` () =
    Assert.False(Lists.is_palindrome [1; 2])

[<Fact>]
let ``Lists.is_palindrome returns true for list with odd number of elements`` () =
    Assert.True(Lists.is_palindrome [1; 2; 3; 2; 1])

[<Fact>]
let ``Lists.is_palindrome returns false for list with odd number of elements`` () =
    Assert.False(Lists.is_palindrome [1..9])

[<Fact>]
let ``Lists.flatten returns same list is list is already flattened`` () =
    Assert.True([1; 2] = Lists.flatten [Lists.Flat 1; Lists.Flat 2])

[<Fact>]
let ``Lists.flatten returns flattened list if list contains one level nesting`` () =
    Assert.True([1; 2; 3] = Lists.flatten [Lists.Flat 1; Lists.Nested [Lists.Flat 2; Lists.Flat 3;]])
