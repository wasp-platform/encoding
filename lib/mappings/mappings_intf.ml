type satisfiability = Satisfiable | Unsatisfiable | Unknown

module type S = sig
  type expr
  type model
  type solver
  type status
  type optimize
  type handle

  exception Error of string

  val encode_expr : Expression.t -> expr
  val expr_to_smtstring : Expression.t list -> Bool.t -> string
  val mk_solver : unit -> solver
  val interrupt : unit -> unit
  val translate : solver -> solver
  val add_solver : solver -> expr List.t -> unit
  val check : solver -> expr List.t -> status
  val get_model : solver -> model Option.t
  val mk_opt : unit -> optimize
  val add_opt : optimize -> expr List.t -> unit
  val maximize : optimize -> expr -> handle
  val minimize : optimize -> expr -> handle
  val get_opt_model : optimize -> model Option.t
  val value_of_const : model -> Expression.t -> Value.t Option.t
  val value_binds : ?symbols:Symbol.t list -> model -> Model.t
  val string_binds : model -> (string * string * string) List.t
  val satisfiability : status -> satisfiability
end
