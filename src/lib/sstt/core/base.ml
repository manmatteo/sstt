
(** @canonical Sstt.Label *)
module Label = Id.NamedIdentifier()
(** Labels used for field names in records. *)

module LabelSet = Hash.SetList(Label)

(** @canonical Sstt.Tag *)
module Tag : sig
  include Id.NamedIdentifier
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool ; preserves_extremum:bool }

  val mk' : string -> prop -> t
  val properties : t -> prop
end = struct
  module I = Id.NamedIdentifier()
  type prop =
  | NoProperty
  | Monotonic of { preserves_cup:bool ; preserves_cap:bool ; preserves_extremum:bool }

  type t = I.t * prop
  let default_prop = Monotonic { preserves_cap=true ; preserves_cup=true ; preserves_extremum=true }
  let mk name =  (I.mk name, default_prop)
  let mk' name prop =  (I.mk name, prop)
  let name (i,_) = I.name i
  let properties (_,p) = p
  let hash (i,_) = I.hash i
  let compare (i1,_) (i2,_) = I.compare i1 i2
  let equal (i1,_) (i2,_) = I.equal i1 i2
  let pp fmt (i,_) = Format.fprintf fmt "%a" I.pp i
  let pp_unique fmt (i,_) = Format.fprintf fmt "%a" I.pp i
end
(** Identifiers used for tagged type. *)


(** @canonical Sstt.Enum *)
module Enum = Id.NamedIdentifier()
(** Identifiers used for enums type. *)


(** @canonical Sstt.Var *)
module Var = Id.NamedIdentifier()
(** Type variables. *)

(** @canonical Sstt.VarSet *)
module VarSet = Set.Make(Var)
(** Sets of type variables. *)

(** @canonical Sstt.VarMap *)
module VarMap = Map.Make(Var)
(** Maps indexed by type variables. *)

(** @canonical Sstt.RowVar *)
(* module RowVar = Id.NamedIdentifier() *)
module RowVar = struct
  module V = Id.NamedIdentifier()
  type t =
  | RowVar of V.t
  | CutVar of (V.t * LabelSet.t)

  let mk name = RowVar (V.mk name)
  let mk_cut name labels = CutVar (V.mk name, labels)
  let compare v1 v2 = match v1, v2 with
  | RowVar v1, RowVar v2 -> V.compare v1 v2
  | CutVar (v1,l1), CutVar (v2,l2) ->
    let c = V.compare v1 v2 in
    if c <> 0 then c else LabelSet.compare l1 l2
  | RowVar _, CutVar _ -> -1
  | CutVar _, RowVar _ -> 1
  let equal v1 v2 = compare v1 v2 = 0
  let hash = function
  | RowVar v -> V.hash v
  | CutVar (v,l) -> Hash.mix (V.hash v) (LabelSet.hash l)
  let pp fmt = function
  | RowVar v -> V.pp fmt v
  | CutVar (v,_) -> V.pp fmt v
end
(** Row variables for polymorphic records. *)

(** @canonical Sstt.VarSet *)
module RowVarSet = Set.Make(RowVar)
(** Sets of row variables. *)

(** @canonical Sstt.FieldVar *)
(* module FieldVar = Id.NamedIdentifier() *)
module FieldVar = struct
  module V = Id.NamedIdentifier()
  type t =
  | FieldVar of V.t
  | OfRow of (RowVar.t * Label.t)
  let mk name = FieldVar (V.mk name)
  let mk_of_rowvar rowv label = OfRow (rowv, label)
  let compare v1 v2 = match v1, v2 with
  | FieldVar v1, FieldVar v2 -> V.compare v1 v2
  | OfRow (r1,l1), OfRow (r2,l2) ->
    let c = RowVar.compare r1 r2 in
    if c <> 0 then c else Label.compare l1 l2
  | FieldVar _, OfRow _ -> -1
  | OfRow _, FieldVar _ -> 1
  let equal v1 v2 = compare v1 v2 = 0
  let hash = function
  | FieldVar v -> V.hash v
  | OfRow (r,l) -> Hash.mix (RowVar.hash r) (Label.hash l)
  let pp fmt = function
  | FieldVar v -> V.pp fmt v
  | OfRow (r,l) -> Format.fprintf fmt "%a.%a" RowVar.pp r Label.pp l

end
(** Field variables for polymorphic records. *)

(** @canonical Sstt.FieldVarSet *)
module FieldVarSet = Set.Make(FieldVar)
(** Sets of field variables. *)
