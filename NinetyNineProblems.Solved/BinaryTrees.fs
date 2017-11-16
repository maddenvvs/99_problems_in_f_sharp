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

    (*
        4.11
        Collect the nodes at a given level in a list.
    *)
    let atlevel k tree =
        if k < 1 then []
        else
            let rec aux m acc = function
                | Empty -> acc
                | Node(_, l, r) as n ->
                    if m = k then (n :: acc)
                    else aux (m + 1) (aux (m + 1) acc l) r
            aux 1 [] tree

    (*
        4.12
        Construct a complete binary tree.
    *)
    let completeBinaryTree n =
        let rec aux = function
            | k when k > n -> Empty
            | k -> Node ('x', (aux (2 * k)), (aux (2 * k + 1)))
        aux 1

    (*
        4.13
        Layout a binary tree (1).
    *)
    let layoutTree1 tree =
        let rec aux depth xPos = function
            | Empty -> (Empty, xPos)
            | Node (v, l, r) ->
                let (l', lx) = aux (depth + 1) xPos l
                let (r', rx) = aux (depth + 1) (lx + 1) r
                (Node ((v, lx, depth), l',r'), rx)
        fst (aux 1 1 tree)

    (*
        4.14
        Layout a binary tree (2).
    *)
    let layoutTree2 tree =
        let rec height = function
            | Empty -> 0
            | Node (_,l,r) -> 1 + max (height l) (height r)

        let treeHeight = height tree

        let rec findLeft depth = function
            | Empty -> treeHeight - depth
            | Node (_, l, _) -> findLeft (depth + 1) l

        let tDst = (1 <<< (findLeft 0 tree)) - 1

        let rec layout depth xRoot = function
            | Empty -> Empty
            | Node (x, l, r) ->
                let spacing = 1 <<< (treeHeight - depth - 1)
                let l' = layout (depth + 1) (xRoot - spacing) l
                let r' = layout (depth + 1) (xRoot + spacing) r
                Node((x, xRoot, depth), l',r')

        layout 1 ((1 <<< (treeHeight - 1)) - tDst) tree

    (*
        4.15
        Layout a binary tree (3).
    *)
    let layoutTree3 tree = failwith "Not implemented"

    (*
        4.16
        A string representation of binary trees.
    *)
    let ``solution for 4.16`` () = failwith "Not implemented"

    (*
        4.17
        Preorder and inorder sequences of binary trees.
    *)
    let ``solution for 4.17`` () = failwith "Not implemented"

    (*
        4.18
        Dotstring representation of binary trees.
    *)
    let ``solution for 4.18`` () = failwith "Not implemented"
