namespace NinetyNineProblems.Solved

module MultiwayTrees =

    type mtree<'a> =
        MTree of 'a * (mtree<'a> list)

    (*
        5.01
        Check whether a given term represents a multiway tree.
    *)
    let isMultiwayTree (MTree _) = true

    (*
        5.02
        Count the nodes of a multiway tree.
    *)
    let rec countNodes (MTree (_, children)) =
        List.fold (fun t child -> t + (countNodes child)) 1 children

    (*
        5.03
        Tree construction from a node string.
    *)
    let constructMTree str = failwith "Not implemented"

    (*
        5.04
        Determine the internal path length of a tree.
    *)
    let rec iplAux len (MTree(_, sub)) =
        List.fold (fun sum t -> sum + iplAux (len + 1) t) len sub

    let ipl t = iplAux 0 t

    (*
        5.05
        Construct the bottom-up order sequence of the tree nodes.
    *)
    let rec prepend_bottom_up (MTree(c, sub)) lst =
        List.foldBack (fun t l -> prepend_bottom_up t l) sub (c :: lst)

    let bottom_up t = prepend_bottom_up t []

    (*
        5.06
        Lisp-like tree representation.
    *)
    let tree_ltl mtree = failwith "Not implemented"
