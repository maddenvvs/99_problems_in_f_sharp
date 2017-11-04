namespace NinetyNineProblems.Solved

module BinaryTrees =

    type btree<'a> =
        | Empty
        | Node of 'a * btree<'a> * btree<'a>

    (*
        4.02
        Construct completely balanced binary trees.
    *)
    let rec cbalTree = function
        | n when n < 2 -> Node ('x', Empty, Empty)
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
        | Empty, Empty -> true
        | Node (_, l1, r1), Node (_, l2, r2) ->
            ((mirror (l1, l2)) && (mirror (r1, r2))) ||
            ((mirror (l1, r2)) && (mirror (r1, l2)))
        | _ -> false

    let symmetric = function
        | Empty -> true
        | Node (_, l, r) -> mirror (l, r)

    (*
        4.04
        Binary search trees (dictionaries).
    *)
    let rec addNode tree x =
        match tree with
        | Empty -> Node (x, Empty, Empty)
        | Node (el, l, r) ->
            if x < el then Node (el, addNode l x, r)
            else Node (el, l, addNode r x)

    let construct arr = List.fold addNode Empty arr
