open System
//module Say 
//namespace TSCN
#r @"C:\Users\111770\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\111770\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"
#load "ast.fsx"
#load "testfile.fsx"
//open FParsec.Primitives
open FParsec
open Ast
 

//this ensures type is resolved
let test p str =
  match run p str with
  | Success(result, _, _)   -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let jUnescapedChar = 
    satisfy (fun ch -> ch <> '\\' && ch <> '\"') <?> "unescaped char"
test jUnescapedChar "\\"

let jEscapedChar = 
    [ 
    // (stringToMatch, resultChar)
    ("\\\"",'\"')      // quote
    ("\\\\",'\\')      // reverse solidus 
    ("\\/",'/')        // solidus
    ("\\b",'\b')       // backspace
    ("\\f",'\f')       // formfeed
    ("\\n",'\n')       // newline
    ("\\r",'\r')       // cr
    ("\\t",'\t')       // tab
    ] 
    // convert each pair into a parser
    |> List.map (fun (toMatch,result) -> 
        pstring toMatch >>% result)
    // and combine them into one
    |> choice
    <?> "escaped char" // set label
test jEscapedChar "\\\\"

let jUnicodeChar = 
    // set up the "primitive" parsers 
    let backslash = pchar '\\'
    let uChar = pchar 'u'
    let hexdigit = anyOf (['0'..'9'] @ ['A'..'F'] @ ['a'..'f'])

    // convert the parser output (nested tuples)
    // to a char
    let convertToChar (((h1,h2),h3),h4) = 
        let str = sprintf "%c%c%c%c" h1 h2 h3 h4
        Int32.Parse(str,Globalization.NumberStyles.HexNumber) |> char

    // set up the main parser
    backslash  >>. uChar >>. hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit
    |>> convertToChar 
test jUnicodeChar "\\u263A"

let jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar 
let quotedString = 
    let quote = pchar '\"' <?> "quote"

    // set up the main parser
    quote >>. manyChars jchar .>> quote 

let pNodeKeys =
        (pstring "type" >>% Type)
    <|> (pstring "name" >>% Name)
    <|> (pstring "parent" >>% Parent) 
    <|> (pstring "instance" >>% Instance)
    <|> (pstring "instance_placeholder" >>% InstancePlaceholder)
    <|> (pstring "owner" >>% Owner)
    <|> (pstring "index" >>% Index)
    <|> (pstring "groups" >>% Groups)
    .>> spaces <?> "heading type" 
test pNodeKeys "name"

// let kvp =  manyTill anyChar (pchar '=') .>>. (quotedString)
// let kvp =  (manyCharsTill jchar (pchar '=') .>>. (quotedString)) |>> KVP
let kvpQuotes =  (pNodeKeys  .>> (pchar '=') .>>. (quotedString) .>> spaces) <?>"node key value pair" |>> (fun (f,s)->f s)

let resoureStart=  (skipManyTill anyChar ( lookAhead(skipChar '['<|> eof )))//manyCharsTill eof //anyChar ((pchar ']' ) <|> (eof ) )
let pHeadingType =
    let left = (pchar '[' .>> spaces) <?> "Resource start"
    let right = pchar ']' .>> spaces
    left
    >>.((pstring "ext_resource" >>% ExtResource)
    <|> (pstring "sub_resource" >>% SubResource)
    <|> (pstring "node"  >>. spaces >>. manyTill kvpQuotes right |>> Node ) 
    <|> (pstring "connection" >>% Connection)
    <|> (pstring "gd_scene" >>% GDScene))
    .>> spaces <?> "heading type"
    .>> manyCharsTill (anyChar) resoureStart

//let pContent = ((manyChars jchar)  .>> (pchar '=') .>>. (manyChars jchar) .>> spaces .>> anyOf[ newline ; eof] )
//test pContent "mesh = SubResource(9)"


let teststring = 
    """[node name="Player" type="Spatial"]             ; The scene root
transform = Transform( 1.0 , 0.0 , -0.0 , 0.0 , 1.0 , -0.0 , -0.0 , -0.0 , 1.0 ,0.0 ,0.0 ,-0.0  )
mesh = SubResource(9)
[node name="Arm" parent="." type="Spatial"]     ; Parented to the scene root
[node name="Hand" parent="Arm" type="Spatial"]
[node name="Finger" parent="Arm/Hand" type="Spatial"]
"""
// let pHeading = 
//     let left = (pchar '[' .>> spaces) <?> "Resource start"
//     let right = pchar ']' .>> spaces
//     // left >>.(pHeadingType .>>. manyTill kvp (pchar ']'))
//     // left >>. pHeadingType .>>. manyTill kvp right .>> manyCharsTill anyChar newline 
//     // left >>. pHeadingType .>>. manyTill kvp right .>> manyCharsTill (anyChar) ((pchar '[') <|> newline )
//     // left >>. pHeadingType .>>. manyTill kvpQuotes right .>> manyCharsTill (anyChar) newline//((pchar '[') )
//     left >>. pHeadingType .>>. manyTill kvpQuotes right .>> manyCharsTill (anyChar) resoureStart//((pchar '[') )

// // test pHeading "[node name=\"Player\" type=\"Spatial\"] ; The scene root"



let  pHeadingContent = pHeadingType  

//test pHeadingContent "sdfasdf["
//test (manyTill pHeading ( (eof) )) teststring

test (manyTill pHeadingType ( (eof) )) TestFile.testfile