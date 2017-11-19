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
