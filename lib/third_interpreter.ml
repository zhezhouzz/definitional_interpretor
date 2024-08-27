open Syntax
open Second_interpreter

(** 3.5th interpreter *)
module HOContInterpreter
    (Exp : EXP)
    (Value : EXP_VALUE with module Exp = Exp with type const = Exp.const) =
struct
  open Value
  open Exp
  open ClosureInterpreter (Exp) (Value)

  type cont = value -> value

  let rec apply (f : funval) (a : value) (c : cont) : value =
    match (f, a) with
    | Closr { lam = { fp; body }; en }, a ->
        eval body (Simp { bvar = fp; bval = a; old = en }) c
    | Sc, a -> c @@ Integer (_value_proj_int a + 1)
    | Eq1, _ -> c @@ mk_eq2 a
    | Eq2 { arg1 }, _ -> c @@ Boolean (equal_value arg1 a)
    | Escf _, _ -> failwith "unimp"

  and eval (r : exp) (e : env) (c : cont) : value =
    match r with
    | Const r -> c @@ evcon r
    | Var r -> c @@ get e r
    | Appl { opr; opnd } ->
        eval opr e (fun f ->
            eval opnd e (fun a -> apply (_value_proj_func f) a c))
    | Lambda lambda -> c @@ mk_closr lambda e
    | Cond { prem; conc; altr } ->
        eval prem e (fun b ->
            if _value_proj_bool b then eval conc e c else eval altr e c)
    | Letrec { dvar; dexp; body } -> eval body (Rec { dvar; dexp; old = e }) c
    | Escape _ -> failwith "unimp"

  let interpret (r : exp) = eval r Init (fun x -> x)
end

(** 3rd interpreter *)
module DefuncContInterpreter
    (Exp : EXP)
    (Value : EXP_VALUE with module Exp = Exp with type const = Exp.const) =
struct
  open Value
  open Exp
  open ClosureInterpreter (Exp) (Value)

  let rec cont cont =
    match cont with
    | Fin -> fun a -> a
    | Evopn { ap = { opnd; _ }; en; next } ->
        fun f -> eval opnd en (Apfun { func = f; next })
    | Apfun { func; next } -> fun a -> apply (_value_proj_func func) a next
    | Branch { cn = { conc; altr; _ }; en; next } ->
        fun b ->
          if _value_proj_bool b then eval conc en next else eval altr en next

  and apply (f : funval) (a : value) (c : cont) : value =
    match (f, a) with
    | Closr { lam = { fp; body }; en }, a ->
        eval body (Simp { bvar = fp; bval = a; old = en }) c
    | Sc, a -> cont c @@ Integer (_value_proj_int a + 1)
    | Eq1, _ -> cont c @@ mk_eq2 a
    | Eq2 { arg1 }, _ -> cont c @@ Boolean (equal_value arg1 a)
    | Escf { cn }, _ -> cont cn a (* we drop orginal continutation c. *)

  and eval (r : exp) (e : env) (c : cont) : value =
    match r with
    | Const r -> cont c @@ evcon r
    | Var r -> cont c @@ get e r
    | Appl appl -> eval appl.opr e (Evopn { ap = appl; en = e; next = c })
    | Lambda lambda -> cont c @@ mk_closr lambda e
    | Cond cond -> eval cond.prem e (Branch { cn = cond; en = e; next = c })
    | Letrec { dvar; dexp; body } -> eval body (Rec { dvar; dexp; old = e }) c
    | Escape { escv; body } ->
        eval body
          (Simp { bvar = escv; bval = Funval (Escf { cn = c }); old = e })
          c

  let interpret (r : exp) = eval r Init Fin
end
