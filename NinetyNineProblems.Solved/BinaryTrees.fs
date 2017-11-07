namespace NinetyNineProblems.Solved

open System

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

    (*
        4.06
        Construct height-balanced binary trees.
    *)
    let rec hbalTree = function
        | n when n < 1 -> [ Empty ]
        | 1 -> [ Node ('x', Empty, Empty) ]
        | n ->
            let t1 = hbalTree (n - 1)
            let t2 = hbalTree (n - 2)
            let eq = generateTrees t1 t1 []
            generateTrees t1 t2 (generateTrees t2 t1 eq)

    (*
        4.07
        Construct height-balanced binary trees with a given number of nodes.
    *)
    let minNodes = function
        | h when h < 1 -> 0
        | h -> 2.0 ** (double (h-1)) |> int

    let maxHeight = function
        | n when n < 1 -> 0
        | n -> Math.Log(double (n+1), 2.0) |> ceil |> int

    let rec countNodesIn = function
        | Empty -> 0
        | Node (_, l, r) -> 1 + (countNodesIn l) + (countNodesIn r)

    let hasNodesEqual n tree =
        countNodesIn tree = n

    let symHbalTrees n =
        n
        |> maxHeight
        |> hbalTree
        |> List.filter (hasNodesEqual n)

    (*
        4.08
        Count the leaves of a binary tree.
    *)
    let rec countLeaves = function
        | Empty -> 0
        | Node(_, Empty, Empty) -> 1
        | Node(_, l, r) -> countLeaves l + countLeaves r

    (*
        4.09
        Collect the leaves of a binary tree in a list.
    *)
    let leaves tree =
        let rec aux acc = function
            | Empty -> []
            | Node(_, Empty, Empty) as n -> n :: acc
            | Node(_, l, r) -> aux (aux acc l) r
        aux [] tree

    (*
        4.10
        Collect the internal nodes of a binary tree in a list.
    *)
    let internals tree =
        let rec aux acc = function
            | Empty -> []
            | Node(_, Empty, Empty) -> acc
            | Node(_, l, r) as n -> aux (aux (n :: acc) l) r
        aux [] tree
