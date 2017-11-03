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

    (*
        4.03
        Symmetric binary trees.
    *)
    let rec mirror = function
        | Leaf _, Leaf _ -> true
        | Node (_, l1, r1), Node (_, l2, r2) ->
            ((mirror (l1, l2)) && (mirror (r1, r2))) ||
            ((mirror (l1, r2)) && (mirror (r1, l2)))
        | _ -> false

    let symmetric = function
        | Leaf _ -> true
        | Node (_, l, r) -> mirror (l, r)
