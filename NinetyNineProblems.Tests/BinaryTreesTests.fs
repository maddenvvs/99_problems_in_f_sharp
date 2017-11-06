module BinaryTreesTests

open Xunit
open NinetyNineProblems.Solved.BinaryTrees

[<Theory>]
[<InlineData(0)>]
[<InlineData(-100)>]
let ``cbalTree generates one-node tree for n < 1`` n =
    Assert.Equal<btree<'a> list>([ Empty ], cbalTree n)

[<Fact>]
let ``symmetric returns true if tree is one leaf only`` () =
    Assert.True(symmetric (Node ('x', Empty, Empty)))

[<Fact>]
let ``symmetric returns true for correct symmetric tree`` () =
    Assert.True(symmetric (Node ('x',
            Node ('x', Node ('x', Empty, Empty), Empty),
            Node ('x', Empty, Node ('x', Empty, Empty))
        )))

[<Fact>]
let ``symmetric returns false for asymmetric tree`` () =
    Assert.False(symmetric (Node ('x',
            Node ('x', Node ('x', Empty, Empty), Empty),
            Node ('x', Node ('x', Empty, Empty), Empty)
        )))

[<Fact>]
let ``construct returns empty tree for empty array`` () =
    Assert.Equal(Empty, construct [])

[<Fact>]
let ``construct returns single node tree for one element array`` () =
    Assert.Equal(Node (1, Empty, Empty), construct [1])

[<Fact>]
let ``construct works correctly for predefined example 1`` () =
    Assert.Equal(Node (3, Node (2, Node (1, Empty, Empty), Empty),
        Node (5, Empty, Node (7, Empty, Empty))),
        construct [3; 2; 5; 7; 1])

[<Fact>]
let ``construct works correctly for predefined example 2`` () =
    Assert.True(symmetric (construct [5; 3; 18; 1; 4; 12; 21]))

[<Fact>]
let ``construct works correctly for predefined example 3`` () =
    Assert.False(symmetric (construct [3; 2; 5; 7; 4]))

[<Theory>]
[<InlineData(2)>]
[<InlineData(4)>]
[<InlineData(30)>]
let ``symCbalTrees generates no symmetric balanced trees for even n`` n =
    Assert.Equal<btree<'a> list>([], symCbalTrees n)

[<Fact>]
let ``symCbalTrees generates 256 trees for n = 57`` () =
    Assert.Equal(256, List.length (symCbalTrees 57))

[<Theory>]
[<InlineData(0)>]
[<InlineData(-1)>]
[<InlineData(-99)>]
let ``hbalTree generates list with empty tree for n < 1`` n =
    Assert.Equal<btree<'a> list>([Empty], hbalTree n)

[<Fact>]
let ``hbalTree generates three trees with height 2`` () =
    Assert.Equal(3, List.length (hbalTree 2))

[<Fact>]
let ``hbalTree generates fifteen trees with height 3`` () =
    Assert.Equal(15, List.length (hbalTree 3))

[<Theory>]
[<InlineData(1, 1)>]
[<InlineData(2, 2)>]
[<InlineData(3, 4)>]
[<InlineData(4, 8)>]
[<InlineData(5, 16)>]
let ``minNodes returns minimum number of nodes in a height-balanced binary tree of height h`` h n =
    Assert.Equal(n, minNodes h)

[<Theory>]
[<InlineData(1, 1)>]
[<InlineData(2, 2)>]
[<InlineData(3, 2)>]
[<InlineData(4, 3)>]
[<InlineData(5, 3)>]
[<InlineData(7, 3)>]
let ``maxHeight returns maximum height of a height-balanced binary tree with n nodes`` n h =
    Assert.Equal(h, maxHeight n)

[<Fact>]
let ``symHbalTrees returns 1 for n = 15`` () =
    Assert.Equal(1, List.length (symHbalTrees 15))