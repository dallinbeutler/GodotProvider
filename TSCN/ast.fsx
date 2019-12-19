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
|Node of NodeKVP List
|Connection
|GDScene 

// type KVP = string * string
// type NodeKVP = NodeKey * string

