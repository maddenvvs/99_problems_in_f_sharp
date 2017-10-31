namespace NinetyNineProblems.Solved

module LogicCodes =

    (*
        3.01, 3.02, 3.03
        Truth tables for logical expressions.
    *)
    type boolExpr =
        | Var of string
        | Not of boolExpr
        | And of boolExpr * boolExpr
        | Or of boolExpr * boolExpr
        | Nand of boolExpr * boolExpr
        | Nor of boolExpr * boolExpr
        | Impl of boolExpr * boolExpr
        | Equ of boolExpr * boolExpr

    let rec evalBoolExpr binds = function
        | Var x -> binds x
        | Not x -> evalBoolExpr binds x
        | And (l, r) -> (evalBoolExpr binds l) && (evalBoolExpr binds r)
        | Or (l, r) -> (evalBoolExpr binds l) || (evalBoolExpr binds r)
        | Nand (l, r) -> not ((evalBoolExpr binds l) && (evalBoolExpr binds r))
        | Nor (l, r) -> not ((evalBoolExpr binds l) || (evalBoolExpr binds r))
        | Impl (l, r) -> not (evalBoolExpr binds l) || (evalBoolExpr binds r)
        | Equ (l, r) -> (evalBoolExpr binds l) = (evalBoolExpr binds r)

    let buildVarFunc projectionList =
        let mp = Map.ofList projectionList
        (fun key -> mp.[key])

    let generateBools len =
        let rec aux t acc = function
            | 0 -> t :: acc
            | l ->
                let res = aux (false :: t) acc (l - 1)
                aux (true :: t) res (l - 1)
        aux [] [] len

    let evalExpr expr projectionList =
        let value = evalBoolExpr (buildVarFunc projectionList) expr
        (projectionList, value)

    let table vars expr =
        List.length vars
        |> generateBools
        |> List.map (List.zip vars)
        |> List.map (evalExpr expr)
