module BinaryTreesTests

open Xunit
open NinetyNineProblems.Solved.BinaryTrees

[<Theory>]
[<InlineData(1)>]
[<InlineData(0)>]
[<InlineData(-100)>]
let ``cbalTree generates one-node tree for n < 2`` n =
    Assert.Equal(Node ('x', Empty, Empty), cbalTree n)

[<Fact>]
let ``cbalTree generates correct tree for n = 3`` () =
    Assert.Equal(Node ('x', Node ('x', Empty, Empty),
        Node ('x', Empty, Empty)), cbalTree 3)

[<Fact>]
let ``cbalTree generates correct tree for n = 7`` () =
    Assert.Equal(Node ('x',
        Node ('x',
            Node ('x', Empty, Empty),
            Node ('x', Empty, Empty)),
        Node ('x',
            Node ('x', Empty, Empty),
            Node ('x', Empty, Empty))),
        cbalTree 7)

[<Fact>]
let ``symmetric returns true if tree is one leaf only`` () =
    Assert.True(symmetric (Node ('x', Empty, Empty)))

[<Theory>]
[<InlineData(3)>]
[<InlineData(7)>]
[<InlineData(15)>]
[<InlineData(31)>]
let ``symmetric returns true for fully balanced trees`` n =
    Assert.True(symmetric (cbalTree n))

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
