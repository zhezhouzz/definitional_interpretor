(** Define Abstract Syntax in Section 4. *)

(** According to the paper, variables and constant are unspecified. *)

module type VAR = sig
  type var [@@deriving show, eq]

  (** Predefined variables*)

  val _succ : var
  val _equal : var
  val _ref : var (* NOTE: for assignment *)
  val _set : var
  val _val : var

  (** Equal *)

  val ( #== ) : var -> var -> bool
end

module type CONST = sig
  type const [@@deriving show, eq]
end

module type EXP = sig
  include VAR
  include CONST

  type exp =
    | Const of const
    | Var of var
    | Appl of appl
    | Lambda of lambda
    | Cond of cond
    | Letrec of letrec
    | Escape of escp (* NOTE: for section 9. *)

  and appl = { opr : exp; opnd : exp }
  and lambda = { fp : var; body : exp }
  and cond = { prem : exp; conc : exp; altr : exp }
  and letrec = { dvar : var; dexp : lambda; body : exp }
  and escp = { escv : var; body : exp } [@@deriving show, eq]
end

module Exp (Var : VAR) (Const : CONST) = struct
  include Var
  include Const

  type exp =
    | Const of const
    | Var of var
    | Appl of appl
    | Lambda of lambda
    | Cond of cond
    | Letrec of letrec
    | Escape of escp (* NOTE: for section 9. *)

  and appl = { opr : exp; opnd : exp }
  and lambda = { fp : var; body : exp }
  and cond = { prem : exp; conc : exp; altr : exp }
  and letrec = { dvar : var; dexp : lambda; body : exp }
  and escp = { escv : var; body : exp } [@@deriving show, eq]
end

module type VALUE = sig
  type const
  type funval

  type value =
    | Integer of int
    | Boolean of bool
    | Funval of funval
        (** Note: we rename "val" into "value" to avoid conflicting with keywords in OCaml. *)

  val _value_proj_bool : value -> bool
  (** Some useful auxiliary functions. The original paper doesn't have these since it uses a set definition instead of inductive datatype. *)

  val _value_proj_int : value -> int
  val _value_proj_func : value -> funval

  val evcon : const -> value
  (** The evcon is required by CONST in original paper, however is not kind of well-found for the second interpreter.  *)
end

(** One possible implementation. *)

module Var = struct
  type var = string [@@deriving show, eq]

  (** Predefined variables*)

  let _succ = "succ"
  let _equal = "zero"
  let _ref = "ref"
  let _set = "set"
  let _val = "val"
  let ( #== ) = equal_var
end

module Const = struct
  type const = Cint of int | Cbool of bool [@@deriving show, eq]
end

module Expression = Exp (Var) (Const)
