open Syntax

module type HO_CONT_VALUE = sig
  type mem = { count : int; possess : int -> value }
  and value = Integer of int | Boolean of bool | Funval of funval | Ref of int
  and cont = mem * value -> value

  and funval = cont -> cont
  (** The original definition is kind of annoying... *)

  val _value_proj_bool : value -> bool
  val _value_proj_int : value -> int
  val _value_proj_func : value -> funval
  val _value_proj_ref : value -> int

  type const

  val evcon : const -> value
  val initmem : mem
  val nextref : mem -> int
  val augment : mem -> value -> mem
  val update : mem -> int -> value -> mem
  val lookup : mem -> int -> value
end

module Value : HO_CONT_VALUE = struct
  type const = Const.const

  type mem = { count : int; possess : int -> value }
  and value = Integer of int | Boolean of bool | Funval of funval | Ref of int
  and cont = mem * value -> value
  and funval = cont -> cont

  let _value_proj_bool = function
    | Boolean b -> b
    | _ -> failwith "the value supposed to be boolean"

  let _value_proj_int = function
    | Integer b -> b
    | _ -> failwith "the value supposed to be int"

  let _value_proj_func = function
    | Funval b -> b
    | _ -> failwith "the value supposed to be lambda function"

  let _value_proj_ref = function
    | Ref b -> b
    | _ -> failwith "the value supposed to be reference"

  open Const

  let evcon = function Cint i -> Integer i | Cbool b -> Boolean b
  let initmem = { count = 0; possess = (fun _ -> Integer 0) }
  let nextref { count; _ } = count + 1

  let augment { count; possess } a =
    {
      count = count + 1;
      possess = (fun x -> if x == count + 1 then a else possess x);
    }

  let update m rf a =
    { m with possess = (fun x -> if x == rf then a else m.possess x) }

  let lookup m rf = m.possess rf
end

module MetaCircularContInterpreter
    (Exp : EXP)
    (Value : HO_CONT_VALUE with type const = Exp.const) =
struct
  open Value
  open Exp

  type env = var -> value

  let rec eval (r : exp) (e : env) (m : mem) (c : cont) : value =
    match r with
    | Const r -> c (m, evcon r)
    | Var r -> c (m, e r)
    | Appl { opr; opnd } ->
        eval opr e m (fun (m, f) ->
            eval opnd e m (fun (m, a) -> (_value_proj_func f) c (m, a)))
    | Lambda lambda -> evlambda lambda e
    | Cond { prem; conc; altr } ->
        eval prem e m (fun (m, b) ->
            if _value_proj_bool b then eval conc e m c else eval altr e m c)
    | Letrec { dvar; dexp; body } ->
        eval body (ext dvar (evlambda dexp e) e) m c
    | Escape { escv; body } -> eval body (ext escv (Funval (fun _ -> c)) e) m c

  and ext z a e x = if x #== z then a else e x

  and evlambda ({ fp; body } : lambda) (e : env) : value =
    Funval (fun c (m, a) -> eval body (ext fp a e) m c)

  let initenv =
    ext _ref (Funval (fun c (m, a) -> c (augment m a, Ref (nextref m))))
    @@ ext _set
         (Funval
            (fun c (m, rf) ->
              c
                ( m,
                  Funval
                    (fun c (m, a) -> c (update m (_value_proj_ref rf) a, a)) )))
    @@ ext _val (Funval (fun c (m, rf) -> c (m, lookup m (_value_proj_ref rf))))
    @@ ext _succ
         (Funval (fun c (m, a) -> c (m, Integer (_value_proj_int a + 1))))
    @@ ext _equal
         (Funval
            (fun c (m, a) ->
              c
                ( m,
                  Funval
                    (fun c (m, b) ->
                      c (m, Boolean (_value_proj_int a == _value_proj_int b)))
                )))
    @@ fun x ->
    failwith (Printf.sprintf "cannot find the given var %s" (show_var x))

  let interpret (r : exp) = eval r initenv initmem (fun (_, x) -> x)
end
