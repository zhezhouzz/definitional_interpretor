open Syntax
(** Section 5. *)

module type HO_VALUE = sig
  type value = Integer of int | Boolean of bool | Funval of funval

  and funval = value -> value
  (** Note: Funval is a function in target language *)

  (** We specialize the signature VALUE here. *)
  include VALUE with type funval := value -> value and type value := value
end

module MetaCircularInterpreter
    (Exp : EXP)
    (Value : HO_VALUE with type const = Exp.const) =
struct
  open Value
  open Exp

  type env = var -> value

  let rec eval (r : exp) (e : env) : value =
    match r with
    | Const r -> evcon r
    | Var r -> e r
    | Appl { opr; opnd } -> (_value_proj_func @@ eval opr e) (eval opnd e)
    | Lambda lambda -> evlambda lambda e
    | Cond { prem; conc; altr } ->
        if _value_proj_bool (eval prem e) then eval conc e else eval altr e
    | Letrec { dvar; dexp; body } ->
        let e' (x : var) = if equal_var x dvar then evlambda dexp e else e x in
        eval body e'

  and evlambda ({ fp; body } : lambda) (e : env) : value =
    Funval
      (fun a ->
        let ext (x : var) : value = if equal_var x fp then a else e x in
        eval body ext)

  let initenv (x : var) =
    if _succ #== x then Funval (fun a -> Integer (_value_proj_int a + 1))
    else if _equal #== x then
      Funval
        (fun a ->
          Funval (fun b -> Boolean (_value_proj_int a == _value_proj_int b)))
      (* Oh, no, we cannot compare two functions! *)
    else failwith (Printf.sprintf "cannot find the given var %s" (show_var x))

  let interpret (r : exp) = eval r initenv
end

(** One possible implementation. *)

module Value = struct
  type const = Const.const

  type value = Integer of int | Boolean of bool | Funval of funval
  and funval = value -> value

  let _value_proj_bool = function
    | Boolean b -> b
    | _ -> failwith "the value supposed to be boolean"

  let _value_proj_int = function
    | Integer b -> b
    | _ -> failwith "the value supposed to be int"

  let _value_proj_func = function
    | Funval b -> b
    | _ -> failwith "the value supposed to be lambda function"

  open Const

  let evcon = function Cint i -> Integer i | Cbool b -> Boolean b
end

module I1 = MetaCircularInterpreter (Expression) (Value)
