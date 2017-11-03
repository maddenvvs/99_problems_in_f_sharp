module BinaryTreesTests

open Xunit
open NinetyNineProblems.Solved.BinaryTrees

[<Theory>]
[<InlineData(1)>]
[<InlineData(0)>]
[<InlineData(-100)>]
let ``cbalTree generates one-node tree for n < 2`` n =
    Assert.Equal(Leaf 'x', cbalTree n)

[<Fact>]
let ``cbalTree generates correct tree for n = 3`` () =
    Assert.Equal(Node ('x', Leaf 'x', Leaf 'x'), cbalTree 3)

[<Fact>]
let ``cbalTree generates correct tree for n = 7`` () =
    Assert.Equal(Node ('x',
        Node ('x', Leaf 'x', Leaf 'x'),
        Node ('x', Leaf 'x', Leaf 'x')), cbalTree 7)

[<Fact>]
let ``symmetric returns true if tree is one leaf only`` () =
    Assert.True(symmetric (Leaf 'x'))

[<Theory>]
[<InlineData(3)>]
[<InlineData(7)>]
[<InlineData(15)>]
[<InlineData(31)>]
let ``symmetric returns true for fully balanced trees`` n =
    Assert.True(symmetric (cbalTree n))