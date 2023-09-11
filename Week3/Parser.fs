module Week3.Parser

// To use FParsec in Fsi
// #I "/Users/au181645/.nuget/packages/fparsec/1.1.1/lib/netstandard2.0"
// #r "FParsecCS.dll"
// #r "FParsec.dll"

open FParsec

type Output = {
    ints: int list list;
    parsedLines: int
}

let addLine (line : int list) (stream : CharStream<Output>) =
    let s = stream.UserState
    stream.UserState <- {s with ints = line::s.ints; parsedLines = s.parsedLines + 1}
    Reply(line)


// Parse a number and consume the following whitespace
let pNumber: Parser<int32, Output> =
    pint32 .>> spaces

// Parse a line
let pLine : Parser<int32 list, Output> =
    manyTill pNumber (pchar '0') .>> newline >>= addLine

// Parse multiple lines until end of file
let pLines : Parser<int32 list list, Output> =
    manyTill pLine eof
    
let input_string= "10 -3 3 0\n4 -4  2 0\n1 2 3 4 5  0\n"

// Default state
let defaultState = { ints = []; parsedLines = 0 }

match runParserOnString pLines defaultState "" input_string with
| Success (result, s, _) -> printfn "%A %A" result s
| Failure(msg, error, _)   -> printfn "%s %A" msg error

