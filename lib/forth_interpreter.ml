open Syntax

module type HO_CONT_VALUE = sig
  type value = Integer of int | Boolean of bool | Funval of funval
  and cont = value -> value

  and funval = value * cont -> value
  (** In mordern CPS, the function is mapping between two continutations (i.e., cont -> cont) *)

  (** We specialize the signature VALUE here. *)
  include
    VALUE with type funval := value * cont -> value and type value := value
end

module MetaCircularContInterpreter
    (Exp : EXP)
    (Value : HO_CONT_VALUE with type const = Exp.const) =
struct
  open Value
  open Exp

  type env = var -> value

  let rec eval (r : exp) (e : env) (c : cont) : value =
    match r with
    | Const r -> c @@ evcon r
    | Var r -> c @@ e r
    | Appl { opr; opnd } ->
        eval opr e (fun f -> eval opnd e (fun a -> (_value_proj_func f) (a, c)))
    | Lambda lambda -> evlambda lambda e
    | Cond { prem; conc; altr } ->
        eval prem e (fun b ->
            if _value_proj_bool b then eval conc e c else eval altr e c)
    | Letrec { dvar; dexp; body } -> eval body (ext dvar (evlambda dexp e) e) c
    | Escape { escv; body } ->
        eval body (ext escv (Funval (fun (a, _) -> c a)) e) c

  and ext z a e x = if x #== z then a else e x

  and evlambda ({ fp; body } : lambda) (e : env) : value =
    Funval
      (fun (a, c) ->
        let ext (x : var) : value = if equal_var x fp then a else e x in
        eval body ext c)

  let initenv (x : var) =
    if _succ #== x then
      Funval (fun (a, c) -> c @@ Integer (_value_proj_int a + 1))
    else if _equal #== x then
      Funval
        (fun (a, c) ->
          c
          @@ Funval
               (fun (b, c) ->
                 c @@ Boolean (_value_proj_int a == _value_proj_int b)))
      (* Oh, no, we cannot compare two functions! *)
    else failwith (Printf.sprintf "cannot find the given var %s" (show_var x))

  let interpret (r : exp) = eval r initenv (fun x -> x)
end
