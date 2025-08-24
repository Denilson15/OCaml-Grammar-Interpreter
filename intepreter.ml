(* parsing util functions *)

let is_lower_case c = 'a' <= c && c <= 'z'
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_alpha c = is_lower_case c || is_upper_case c
let is_digit c = '0' <= c && c <= '9'
let is_alphanum c = is_lower_case c || is_upper_case c || is_digit c
let is_blank c = String.contains " \012\n\r\t" c

let explode s = List.of_seq (String.to_seq s)
let implode ls = String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ loop ()
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)
type 'a parser = char list -> ('a * char list) option
let parse (p : 'a parser) (s : string) : ('a * char list) option = p (explode s)
let pure (x : 'a) : 'a parser = fun ls -> Some (x, ls)
let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls -> match p ls with
            Some (a, ls) -> q a ls
            |
            None -> None

let ( >>= ) = bind
let ( let* ) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls -> if f x then Some (x, ls)
               else None
  | _ -> None

let char (c : char) : char parser = satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let ( >> ) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> (
      match p2 ls with
      | Some (_, ls) -> Some (x, ls)
      | None -> None)
  | None -> None

let ( << ) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) -> Some (x, ls)
  | None -> p2 ls

let ( <|> ) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let ( >|= ) = map

let ( >| ) p c = map p (fun _ -> c)

let rec many (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : 'a list parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> (
      match many p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : 'a list parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) -> (
      match many' p ls with
      | Some (xs, ls) -> Some (x :: xs, ls)
      | None -> Some ([ x ], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c then
      Some ((), ls)
    else
      None
  | _ -> None

let ws : unit parser = many whitespace >| ()
let ws1 : unit parser = many1 whitespace >| ()
let digit : char parser = satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) -> Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
    let cs = explode s in
      let rec loop cs ls =
        match (cs, ls) with
        | [], _ -> Some ((), ls)
        | c :: cs, x :: xs ->
          if x = c then
            loop cs xs
          else
            None
        | _ -> None
      in
    loop cs ls

let keyword (s : string) : unit parser = literal s >> ws >| ()
let return = pure
(* end of parser combinators *)

(* Assignment Logic *)

(* Grammar *)
type const = Int of int | Bool of bool | Unit
type coms = Push of const | Pop of int | Trace of int | Add of int | Sub of int | Mul of int | Div of int
type prog = coms list
type stack = const list


(* type const = Int of int | Bool of bool | Unit *)
let naturalNumberParser ls = 
    match (many1 digit ls) with 
    None -> None
    |
    Some (naturalNumberDigits, unconsumedString) -> Some (int_of_string(implode naturalNumberDigits), unconsumedString)

let intParser = 
  naturalNumberParser <|>
  (
    satisfy (fun x -> x = '-') >>= fun _ ->
    naturalNumberParser >>= fun n ->
    return (-1 * n)
  )

let constIntParser = 
  intParser >>= fun x -> 
  return (Int x)

let boolParser = 
  (ws >>= fun _ ->
  literal "True" >>= fun _ ->
  return (Bool true))
  <|>
  (ws >>= fun _ ->
  literal "False" >>= fun _ -> 
  return (Bool false))

let unitParser = 
  ws >>= fun _ ->
    literal "()" >>= fun _ ->
    return (Unit) 

let constParser = constIntParser <|> boolParser <|> unitParser

(* type coms = Push of const | Pop of int | Trace of int | Add of int | Sub of int | Mul of int | Div of int *)

let pushParser = 
ws >>= fun _ ->
literal "Push" >>= fun _ ->
ws >>= fun _ ->
constParser >>= fun c ->
return (Push c) 

let popParser = 
ws >>= fun _ ->
literal "Pop" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Pop i)

let addParser = 
ws >>= fun _ ->
literal "Add" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Add i)

let subParser = 
ws >>= fun _ ->
literal "Sub" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Sub i)

let mulParser = 
ws >>= fun _ ->
literal "Mul" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Mul i)

let divParser = 
ws >>= fun _ ->
literal "Div" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Div i)

let traceParser = 
ws >>= fun _ ->
literal "Trace" >>= fun _ ->
ws >>= fun _ -> 
intParser >>= fun i ->
return (Trace i)

let convertConstToString (c:const) = 
    match c with 
      Int i -> (string_of_int i)
      |
      Bool b -> if b = true then "True" else "False"
      |
      Unit -> "()"

let rec backwardsTrace (n:int) (s:stack) =
  if n < 0 then None
  else if n > 0 && s = [] then None 
  else if n = 0 then Some (s, [])
  else
    match s with
    const::rest -> match backwardsTrace (n-1) rest with 
                      Some (updStack, accum) -> Some (updStack, ((convertConstToString const)::accum))
                      |
                      None -> None

let trace n s = 
  match (backwardsTrace n s) with
  Some (remainingStack, log) -> Some (remainingStack, List.rev log)
  |
  None -> None
                    
let add n s = 
  if n < 0 then None
  else if n = 0 then Some ((Int 0)::s)
  else
    let rec aux n s sum = 
      match n, s with
      0, rest -> Some ((Int sum) :: rest)
      |
      n, (Int h)::t -> aux (n-1) t (h+sum)
      |
      _ -> None
    in aux n s 0

let sub n s = 
  if n < 0 then None
  else if n = 0 then Some((Int 0)::s)
  else 
  match s with
    (Int top) ::rest ->
      let rec aux n s accum = 
        match n, s with
        0, remainingStack -> Some (Int (top-accum)::remainingStack)
        |
        n, (Int h)::t -> aux (n-1) t (accum + h)
        |
        _ -> None
      in aux (n-1) rest 0
    |
      _ -> None

let mul n s = 
  if n < 0 then None
  else if n = 0 then Some((Int 1)::s)
  else 
    let rec aux n s prod = 
      match n, s with
      0, rest -> Some((Int prod) :: rest)
      |
      n, (Int h)::t -> aux (n-1) t (h*prod)
      |
      _ -> None
    in aux n s 1

let div n s = 
  if n < 0 then None
  else if n = 0 then Some((Int 1)::s)
  else if n = 1 then 
  match s with
    (Int top) ::rest -> Some((Int top)::rest)
    |
    _ -> None
  else   
    match s with
    (Int top) ::rest -> 
    let rec aux n s accum = 
      match n, s with
      0, remainingStack -> if accum = 0 then None
                            else Some(Int (top/accum)::remainingStack)
      |
      n, (Int h)::t -> aux (n-1) t (accum * h)
      |
      _ -> None
    in aux (n-1) rest 1
  |
    _ -> None


let pop n s = 
  if n < 0 then None
  else if n = 0 then Some s
  else 
    let rec aux n s = 
      match n, s with
      0, remainingStack -> Some remainingStack
      |
      n, h::t -> aux (n-1) t
      |
      _ -> None
    in aux n s

let commandParser = pushParser <|> popParser <|> addParser <|> subParser <|> mulParser <|> divParser <|> traceParser

(* type prog = coms list *)
let progParser = many commandParser


(* Main Function *)
let interp (src : string) : string list = 
  match (parse progParser src) with
  Some (commands, unconsumedString) -> 
    let rec runCommands comms stack log = 
      (match comms with
        cmd::remainingCommands -> 
          (match cmd with
            Push const -> let updStack = const::stack in runCommands remainingCommands updStack log
            |
            Pop num -> 
              (match (pop num stack) with
              Some updStack -> runCommands remainingCommands updStack (log)
              |
              None -> ["Error"])
            |
            Trace num -> 
              (match(trace num stack) with
                Some (updStack, logEntries) -> runCommands remainingCommands updStack (logEntries::log)
                |
                None -> ["Error"])
                        
            |
            Add num -> 
              (match(add num stack) with
                Some updStack -> runCommands remainingCommands updStack (log)
                |
                None -> ["Error"]) 
            |
            Sub num -> 
              (match(sub num stack) with
                Some updStack -> runCommands remainingCommands updStack (log)
                |
                None -> ["Error"])
                        
            |
            Mul num -> 
              (match(mul num stack) with
                Some updStack -> runCommands remainingCommands updStack (log)
                |
                None -> ["Error"])
                        
            |
            Div num -> 
              (match(div num stack) with
                Some updStack -> runCommands remainingCommands updStack (log)
                |
                None -> ["Error"])
          )                          
        |
        [] -> List.flatten log
      )
    in runCommands commands [] []
  |
  None -> ["Error"]

(* Calling (main "test.txt") will read the file test.txt and run interp on it.
   This is only used for debugging and will not be used by the gradescope autograder. *)
let main fname =
  let src = readlines fname in
  interp src