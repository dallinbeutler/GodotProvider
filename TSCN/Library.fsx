//module Say 
//namespace TSCN
#r @"C:\Users\111770\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsecCS.dll"
#r @"C:\Users\111770\.nuget\packages\fparsec\1.0.3\lib\netstandard1.6\FParsec.dll"
//open FParsec.Primitives
open FParsec

let hello name =
  printfn "Hello %s" name

type JValue = 
  | JString of string
  | JNumber of float
  | JBool   of bool
  | JNull
  | JObject of Map<string, JValue>
  | JArray  of JValue list

//// applies the parser p, ignores the result, and returns x.
//let (>>%) p x =
//    p |>> (fun _ -> x)

let jNull = 
  pstring "null" 
  >>% JNull 
  <?> "null"

let jBool =   
    let jtrue = 
        pstring "true" 
        >>% JBool true   // map to JBool
    let jfalse = 
        pstring "false" 
        >>% JBool false  // map to JBool 

    // choose between true and false
    jtrue <|> jfalse
    <?> "bool"           // give it a label


let jUnescapedChar = 
  satisfy (fun ch -> ch <> '\\' && ch <> '\"') ""


/// Parse an escaped char
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








//this ensures type is resolved
let test p str =
  match run p str with
  | Success(result, _, _)   -> printfn "Success: %A" result
  | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

test jNull "null"
test jBool "true"
run jEscapedChar "\\\\" // Success '\'
run jEscapedChar "\\t"  // Success '\009'

//run jEscapedChar "a" |> printResult
//run jUnescapedChar " "
//