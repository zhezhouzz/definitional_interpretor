open Syntax
(** Section 6. *)

module type EXP_VALUE = sig
  module Exp : EXP

  type value = Integer of int | Boolean of bool | Funval of funval

  and funval =
    | Closr of { lam : Exp.lambda; en : env }
    | Sc
    | Eq1
    | Eq2 of { arg1 : value }
    | Escf of { cn : cont }

  and env =
    | Init
    | Simp of { bvar : Exp.var; bval : value; old : env }
    | Rec of { dvar : Exp.var; dexp : Exp.lambda; old : env }

  and cont =
    | Fin
    | Evopn of { ap : Exp.appl; en : env; next : cont }
    | Apfun of { func : value; next : cont }
    | Branch of { cn : Exp.cond; en : env; next : cont }
  [@@deriving show, eq]

  (** We specialize the signature VALUE here. *)
  include VALUE with type funval := funval and type value := value
end

module ClosureInterpreter
    (Exp : EXP)
    (Value : EXP_VALUE with module Exp = Exp with type const = Exp.const) =
struct
  open Value
  open Exp

  let mk_closr lam en = Funval (Closr { lam; en })
  let mk_eq2 (arg1 : value) = Funval (Eq2 { arg1 })
  let mk_eq1 = Funval Eq1
  let mk_sc = Funval Sc

  let initenv (x : var) =
    if _succ #== x then mk_sc
    else if _equal #== x then mk_eq1
    else failwith (Printf.sprintf "cannot find the given var %s" (show_var x))

  let rec get (e : env) (r : var) =
    match e with
    | Init -> initenv r
    | Simp { bvar; bval; old } -> if r #== bvar then bval else get old r
    | Rec { dvar; dexp; old } ->
        if r #== dvar then mk_closr dexp old else get old r

  let rec apply (f : funval) (a : value) : value =
    match (f, a) with
    | Closr { lam = { fp; body }; en }, a ->
        eval body (Simp { bvar = fp; bval = a; old = en })
    | Sc, a -> Integer (_value_proj_int a + 1)
    | Eq1, _ -> mk_eq2 a
    | Eq2 { arg1 }, _ -> Boolean (equal_value arg1 a)
    | Escf _, _ -> failwith "cannot have escape!"
  (* Now we can compare two values! *)

  and eval (r : exp) (e : env) : value =
    match r with
    | Const r -> evcon r
    | Var r -> get e r
    | Appl { opr; opnd } -> apply (_value_proj_func @@ eval opr e) (eval opnd e)
    | Lambda lambda -> mk_closr lambda e
    | Cond { prem; conc; altr } ->
        if _value_proj_bool (eval prem e) then eval conc e else eval altr e
    | Letrec { dvar; dexp; body } -> eval body (Rec { dvar; dexp; old = e })
    | Escape _ -> failwith "cannot have escape!"

  let interpret (r : exp) = eval r Init
end

(** One possible implementation. *)

module Value = struct
  type const = Expression.const

  module Exp = Expression
  open Expression

  type value = Integer of int | Boolean of bool | Funval of funval

  and funval =
    | Closr of { lam : lambda; en : env }
    | Sc
    | Eq1
    | Eq2 of { arg1 : value }
    | Escf of { cn : cont }

  and env =
    | Init
    | Simp of { bvar : var; bval : value; old : env }
    | Rec of { dvar : var; dexp : lambda; old : env }

  and cont =
    | Fin
    | Evopn of { ap : appl; en : env; next : cont }
    | Apfun of { func : value; next : cont }
    | Branch of { cn : cond; en : env; next : cont }
  [@@deriving show, eq]

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

module I2 = ClosureInterpreter (Expression) (Value)
