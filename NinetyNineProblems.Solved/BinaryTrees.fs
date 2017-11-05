namespace NinetyNineProblems.Solved

module BinaryTrees =

    type btree<'a> =
        | Empty
        | Node of 'a * btree<'a> * btree<'a>

    (*
        4.02
        Construct completely balanced binary trees.
    *)
    let generateTrees l r acc =
        let addRight acc l =
            List.fold (fun a r -> Node ('x', l, r) :: a) acc r
        List.fold addRight acc l

    let rec cbalTree = function
        | n when n < 1 -> [ Empty ]
        | n when n % 2 = 1 ->
            let b = cbalTree (n / 2)
            generateTrees b b []
        | n ->
            let b1 = cbalTree (n / 2)
            let b2 = cbalTree (n / 2 - 1)
            generateTrees b1 b2 (generateTrees b2 b1 [])

    (*
        4.03
        Symmetric binary trees.
    *)
    let rec mirror = function
        | Empty, Empty -> true
        | Node (_, l1, r1), Node (_, l2, r2) ->
            mirror (l1, r2) && mirror (r1, l2)
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

    (*
        4.05
        Apply the generate-and-test paradigm to construct all symmetric,
        completely balanced binary trees with a given number of nodes.
    *)
    let symCbalTrees n = List.filter symmetric (cbalTree n)
