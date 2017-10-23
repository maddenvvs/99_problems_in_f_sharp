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

[<Fact>]
let ``Lists.compress returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.compress [])

[<Fact>]
let ``Lists.compress returns list with two elements if initial list consists of two unique elements`` () =
    Assert.True([1; 2] = Lists.compress [1; 2])

[<Fact>]
let ``Lists.compress removes duplicates leading to one element's list`` () =
    Assert.True([1] = Lists.compress [1; 1; 1; 1; 1])

[<Fact>]
let ``Lists.compress works correctly with huge list with no duplicates`` () =
    Assert.True([1..1000000] = Lists.compress [1..1000000])

[<Fact>]
let ``Lists.compress works correctly with complicated case`` () =
    Assert.True([1; 2; 4; 1] = Lists.compress [1; 1; 1; 1; 2; 4; 4; 1; 1])

[<Fact>]
let ``Lists.pack returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.pack [])

[<Fact>]
let ``Lists.pack returns packed list with ones`` () =
    Assert.True([[1; 1; 1]] = Lists.pack [1; 1; 1])

[<Fact>]
let ``Lists.pack returns packed list with ones and twos`` () =
    Assert.True([[1; 1; 1]; [2; 2; 2]] = Lists.pack [1; 1; 1; 2; 2; 2])

[<Fact>]
let ``Lists.pack returns packed list for difficult test`` () =
    Assert.True([["a";"a";"a";"a"];["b"];["c";"c"];["a";"a"];["d"];["e";"e";"e";"e"]] =
        Lists.pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])

[<Fact>]
let ``Lists.pack returns packed list for huge list`` () =
    Assert.True([for i in 1..1000000 -> [i]] =
        Lists.pack [1..1000000])

[<Fact>]
let ``Lists.encode returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.encode [])

[<Fact>]
let ``Lists.encode returns packed list with ones`` () =
    Assert.True([(3,1)] = Lists.encode [1; 1; 1])

[<Fact>]
let ``Lists.encode returns packed list with ones and twos`` () =
    Assert.True([(3,1); (2,2)] = Lists.encode [1; 1; 1; 2; 2])

[<Fact>]
let ``Lists.encode returns packed list for difficult test`` () =
    Assert.True([(4,"a"); (1,"b"); (2,"c"); (2,"a"); (1,"d"); (4,"e")] =
        Lists.encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"])

[<Fact>]
let ``Lists.encode returns packed list for huge list`` () =
    Assert.True([for i in 1..1000000 -> (1,i)] =
        Lists.encode [1..1000000])

[<Fact>]
let ``Lists.encode_modified returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.encode_modified [])

[<Fact>]
let ``Lists.encode_modified returns packed list with ones`` () =
    Assert.True([Lists.Many (3,1)] = Lists.encode_modified [1; 1; 1])

[<Fact>]
let ``Lists.encode_modified returns packed list with ones, twos and one three`` () =
    Assert.True([Lists.Many (3,1); Lists.Many (2,2); Lists.One 3] =
        Lists.encode_modified [1; 1; 1; 2; 2; 3])

[<Fact>]
let ``Lists.encode_modified returns packed list for huge list`` () =
    Assert.True([for i in 1..1000000 -> Lists.One i] =
        Lists.encode_modified [1..1000000])

[<Fact>]
let ``Lists.decode_modified returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.decode_modified [])

[<Fact>]
let ``Lists.decode_modified decoded list with one element`` () =
    Assert.True([1] = Lists.decode_modified [Lists.One 1])

[<Fact>]
let ``Lists.decode_modified decoded list with many equal elements`` () =
    Assert.True([1; 1; 1; 1; 1] = Lists.decode_modified [Lists.Many (5, 1)])

[<Fact>]
let ``Lists.decode_modified decoded list with different types of elements`` () =
    Assert.True([1; 1; 2; 3; 3] =
        Lists.decode_modified [Lists.Many (2, 1); Lists.One 2; Lists.Many (2, 3)])

[<Fact>]
let ``Lists.decode_modified decoded huge list`` () =
    Assert.True([for i in 1..1000000 -> i] =
        Lists.decode_modified [for i in 1..1000000 -> Lists.One i])

[<Fact>]
let ``Lists.duplicate returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.duplicate [])

[<Fact>]
let ``Lists.duplicate returns 2 elements if initial list contains 1 element`` () =
    Assert.True([1; 1] = Lists.duplicate [1])

[<Fact>]
let ``Lists.duplicate returns 6 elements if initial list contains 3 elements`` () =
    Assert.True([1; 1; 2; 2; 3; 3] = Lists.duplicate [1; 2; 3])

[<Fact>]
let ``Lists.duplicate works correctly with huge lists`` () =
    Assert.True([for i in 1..100000 -> 1] = Lists.duplicate [for i in 1..50000 -> 1])

[<Fact>]
let ``Lists.nduplicate returns empty list if initial list is empty`` () =
    Assert.True([] = Lists.nduplicate 1u [])

[<Fact>]
let ``Lists.nduplicate returns empty list if initial count = 0`` () =
    Assert.True([] = Lists.nduplicate 0u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.nduplicate returns 2 elements if initial list contains 1 element`` () =
    Assert.True([1; 1] = Lists.nduplicate 2u [1])

[<Fact>]
let ``Lists.nduplicate returns 9 elements if initial list contains 3 elements`` () =
    Assert.True([1; 1; 1; 2; 2; 2; 3; 3; 3] = Lists.nduplicate 3u [1; 2; 3])

[<Fact>]
let ``Lists.nduplicate works correctly with huge lists`` () =
    Assert.True([for i in 1..100000 -> 1] = Lists.nduplicate 10u [for i in 1..10000 -> 1])

[<Fact>]
let ``Lists.drop returns empty list when empty list provided`` () =
    Assert.True([] = Lists.drop 1u [])

[<Fact>]
let ``Lists.drop doesn't remove anything if drop ever 0 element`` () =
    Assert.True([1; 2; 3; 4; 5] = Lists.drop 0u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.drop wipes all array if drop ever 1 element`` () =
    Assert.True([] = Lists.drop 1u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.drop returns unchanged array if drop is more then length or array`` () =
    Assert.True([1; 2; 3; 4; 5] = Lists.drop 6u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.drop works correctly for short list`` () =
    Assert.True([1; 3; 5] = Lists.drop 2u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.drop works correctly for huge list`` () =
    Assert.True([for i in 1..90000 -> 1] =
        Lists.drop 10u [for i in 1..100000 -> 1])

[<Fact>]
let ``Lists.split works correctly with empty list`` () =
    Assert.True(([], []) = Lists.split 1u [])

[<Fact>]
let ``Lists.split returns empty left part if index is zero`` () =
    Assert.True(([], [1; 2; 3]) = Lists.split 0u [1; 2; 3])

[<Fact>]
let ``Lists.split returns empty right part if index is equal to length of list`` () =
    Assert.True(([1; 2; 3], []) = Lists.split 3u [1; 2; 3])

[<Fact>]
let ``Lists.split splits properly if index is between both sides`` () =
    Assert.True(([1; 2], [3; 4; 5]) = Lists.split 2u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.split splits properly if list is huge`` () =
    Assert.True(([for i in 1..10000 -> 1], [for i in 1..10000 -> 1]) =
        Lists.split 10000u [for i in 1..20000 -> 1])

[<Fact>]
let ``Lists.slice works correctly with empty list`` () =
    Assert.True([] = Lists.slice 1u 2u [])

[<Fact>]
let ``Lists.slice returns whole list if slice is equal to list length`` () =
    Assert.True([1; 2; 3; 4] = Lists.slice 1u 4u [1; 2; 3; 4])

[<Fact>]
let ``Lists.slice returns whole list if slice is larger then list length`` () =
    Assert.True([1; 2; 3; 4] = Lists.slice 0u 100u [1; 2; 3; 4])

[<Fact>]
let ``Lists.slice returns middle part of the list`` () =
    Assert.True([2; 3; 4] = Lists.slice 2u 4u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.slice returns empty list if slice borders are reversed`` () =
    Assert.True([] = Lists.slice 4u 2u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.slice works correctly with huge list`` () =
    Assert.True([for i in 500..95000 -> i] =
        Lists.slice 500u 95000u [for i in 1..100000 -> i])

[<Fact>]
let ``Lists.rotate works correctly with empty list`` () =
    Assert.True([] = Lists.rotate 2 [])

[<Fact>]
let ``Lists.rotate works correctly with positive shift`` () =
    Assert.True([3; 4; 1; 2] = Lists.rotate 2 [1; 2; 3; 4])

[<Fact>]
let ``Lists.rotate works correctly with negative shift`` () =
    Assert.True([2; 3; 4; 1] = Lists.rotate -3 [1; 2; 3; 4])

[<Fact>]
let ``Lists.rotate works correctly with zero`` () =
    Assert.True([2; 3; 4; 1] = Lists.rotate 0 [2; 3; 4; 1])

[<Fact>]
let ``Lists.rotate works correctly with huge lists`` () =
    Assert.True([for i in 5001..10000 -> i] @ [for i in 1..5000 -> i] =
        Lists.rotate 5000 [for i in 1..10000 -> i])

[<Fact>]
let ``Lists.remove_at works with empty list`` () =
    Assert.True((None, []) = Lists.remove_at 3u [])

[<Fact>]
let ``Lists.remove_at works if element to remove is inside of list`` () =
    Assert.True((Some 4, [1; 2; 3; 5]) = Lists.remove_at 4u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.remove_at works if element to remove is out of boundary`` () =
    Assert.True((None, [1; 2; 3; 4; 5]) = Lists.remove_at 100u [1; 2; 3; 4; 5])

[<Fact>]
let ``Lists.remove_at works if list is huge`` () =
    Assert.True((Some 1, [for i in 1..9999 -> 1]) =
        Lists.remove_at 9999u [for i in 1..10000 -> 1])

[<Fact>]
let ``Lists.insert_at works well with empty lists`` () =
    Assert.True([1] = Lists.insert_at 1 1u [])

[<Fact>]
let ``Lists.insert_at inserts in the middle of list`` () =
    Assert.True([1; 2; 3; 4] = Lists.insert_at 3 3u [1; 2; 4])

[<Fact>]
let ``Lists.insert_at inserts in the end of list if index is larger the list length`` () =
    Assert.True([1; 2; 3] = Lists.insert_at 3 100u [1; 2])

[<Fact>]
let ``Lists.insert_at works well with huge lists`` () =
    Assert.True([for i in 1..10000 -> 1] =
        Lists.insert_at 1 9998u [for i in 1..9999 -> 1])
