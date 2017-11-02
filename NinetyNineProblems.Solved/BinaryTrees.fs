namespace NinetyNineProblems.Solved

module BinaryTrees =

    type btree<'a> =
        | Leaf of 'a
        | Node of 'a * btree<'a> * btree<'a>

    (*
        4.02
        Construct completely balanced binary trees.
    *)
    let rec cbalTree = function
        | n when n < 2 -> Leaf 'x'
        | n ->
            let reminded = n - 1
            Node (
                'x',
                cbalTree (reminded / 2),
                cbalTree (reminded - reminded / 2)
            )
