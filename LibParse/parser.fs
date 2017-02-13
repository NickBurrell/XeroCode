type Result<'TSuccess, 'TFailure> =
  | Success of 'TSuccess
  | Failure of 'TFailure

let explode s =
  [for c in s -> c]

let implode (xs:char list) =
  let sb = System.Text.StringBuilder(xs.Length)
  xs |> List.iter (sb.Append >> ignore)
  sb.ToString()

// Parser

type Parser<'a> =
  Parser of (char list -> Result<'a * char list, string>)

let runParser parser inputChars =
  let (Parser parserFunc) = parser
  parserFunc inputChars

let expectChar expectedChar =
  let innerParser inputChars =
    match inputChars with
    | c :: remainingChars ->
      if c = expectedChar then Success (c, remainingChars)
      else Failure (sprintf "Expected '%c', got '%c'" expectedChar c)
    |  [] ->
      Failure (sprintf "Expected '%c', reached end of input" expectedChar)
  Parser innerParser

let orParse parser1 parser2 =
  let innerParser inputChars =
    match runParser parser1 inputChars with
    | Success result -> Success result
    | Failure _ -> runParser parser2 inputChars
  Parser innerParser

let (<|>) = orParse

let choice parserList =
  List.reduce orParse parserList

let anyCharOf validChars =
  validChars
  |> List.map expectChar
  |> choice

let andParse parser1 parser2 =
  let innerParser inputChars =
    match runParser parser1 inputChars with
    | Failure  msg -> Failure msg
    | Success (c1, remaining1) ->
      match runParser parser2 remaining1 with
      | Failure msg -> Failure msg
      | Success (c2, remaining2) ->
        Success ((c1, c2), remaining2)

  Parser innerParser

let ( .>>. ) = andParse

let mapParser mapFunc parser =
  let innerFunc inputChars =
    match runParser parser inputChars with
    | Failure msg -> Failure msg
    | Success (result, remaining) ->
      Success (mapFunc result, remaining)
  Parser innerFunc

let rec sequenceParsers (parserList: Parser<'a> list): Parser<'a list> =
  match parserList with
  | [] -> []
  | parser :: remainingParsers ->
    parser :: sequenceParsers remainingParsers

let expectString expectedString =
  expectedString
  |> explode
  |> List.map expectChar
  |> List.reduce andParse

explode "take"
|> runParser(expectChar 't')
|> printfn "%A"
