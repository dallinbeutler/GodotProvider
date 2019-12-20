module Ast

type NodeKVP = 
|Type of string
|Name of string
|Parent of string
|Instance of string
|InstancePlaceholder of string
|Owner of string
|Index of string
|Groups of string

type ResourceType =
|ExtResource
|SubResource
|NodeResource of NodeKVP List
|Connection
|GDScene 

let isParentNode kvpList =   
    match kvpList |>
        List.tryFind(
            fun kvp->
            match kvp with
            |Parent _ -> true
            |_ -> false     ) with
        | Some _ -> false
        | None -> true
    

type SimpleParseNode = {
    Name:string 
    Type:string
    Parent:string option
}

let formSimpleParsed kvpList =
    let mutable node = {Name =""; Type = ""; Parent = None }
    kvpList |> List.fold (fun n kvp ->
        match kvp with 
        |Name name-> {n with Name = name}
        |Type t-> {n with Type = t}
        |Parent p-> {n with Parent = Some p}
        |_-> n
        ) node




// // type KVP = string * string
// // type NodeKVP = NodeKey * string
// type TreeNode ={
//     Name:string
//     Type : string
//     Parent : TreeNode option
// }

// let formTree (spnl:SimpleParseNode list) =
//     let convertRoot (n:SimpleParseNode) = {Name = n.Name; Type = n.Type;  Parent = None}
//     let convertNode (n:SimpleParseNode) (parent) = {Name = n.Name; Type = n.Type; Parent = Some parent}
//     //let simpleNodes = spnl |> List.map convertNode
//     let root = spnl.[0] |> convertRoot //|> List.find (fun n -> n.Parent = None) |> convertRoot
    
//     let children = 
//         spnl.[1..]
//         |> List.fold (
//             fun (newList:TreeNode list) node -> 
//             match node.Parent with 
//             |None -> newList 
//             |Some pName -> 
//                 match pName with 
//                 | "." -> convertNode node root :: newList
//                 | path -> convertNode node (newList |> List.find (fun n -> n.Name = parent)) :: newList
//                 ) [root]
//     children
    