open TinyCaml

let parse_string str =
  let lexbuf = Lexing.from_string str in
  Parser.ast Lexer.read_token lexbuf

let eval_string str =
  let ast = parse_string str in
  Eval.eval Eval.empty_env ast

let value_testable =
  Alcotest.testable
    (fun fmt v -> Format.fprintf fmt "%s" (Eval.string_of_value v))
    (fun a b ->
      match (a, b) with
      | Eval.VNumber x, Eval.VNumber y -> Float.(abs (x -. y) < 0.01)
      | Eval.VBool x, Eval.VBool y -> Bool.equal x y
      | _ -> false)

let test_example name expected =
  let program =
    {|let x = 10 in
let x = x + 5 in
let x = x * 2 in

let a = 3.14 in
let b = 2.0 in
let floatResult = a * b * b in

let t = true in
let f = false in
let boolResult = t && !f || f in

let lt = 5 < 10 in
let gt = 10 > 5 in
let eq = 5 == 5 in
let neq = 5 != 10 in
let lte = 5 <= 5 in
let gte = 10 >= 10 in
let allComparisons = lt && gt && eq && neq && lte && gte in

let double = fun n -> n * 2 in
let apply = fun f -> fun x -> f x in
let compose = fun f -> fun g -> fun x -> f (g x) in
let quadruple = compose double double in

let n = 5 in
let n = double n in
let n = apply double n in

let factorial = fun self ->
  fun n ->
    if n <= 1 then 1 else n * self self (n + -1)
in
let fact5 = factorial factorial 5 in

let innerShadow =
  let x = 100 in
  let x = x + 1 in
  x
in

let closureTest =
  let multiplier = 10 in
  let mult = fun x -> x * multiplier in
  mult 5
in

if allComparisons && boolResult then
  x + floatResult + n + fact5 + innerShadow + closureTest + quadruple 3
else
  0|}
  in
  let result = eval_string program in
  Alcotest.(check value_testable) name expected result

(* comprehensive.tiny tests:
   - Shadowing: x = 10 -> 15 -> 30
   - Float arithmetic: 3.14 * 2.0 * 2.0 = 12.56
   - Boolean ops: true && !false || false = true
   - All comparisons: all true
   - Functions: double, apply, compose, quadruple
   - n = 5 -> 10 -> 20
   - Recursion: factorial 5 = 120
   - Inner shadowing: 100 -> 101
   - Closure: 10 * 5 = 50
   - quadruple 3 = 12
   Result: 30 + 12.56 + 20 + 120 + 101 + 50 + 12 = 345.56 *)
let test_comprehensive () =
  test_example "comprehensive.tiny" (Eval.VNumber 345.56)

let () =
  Alcotest.run "TinyCaml"
    [
      ( "examples",
        [ Alcotest.test_case "comprehensive" `Quick test_comprehensive ] );
    ]
