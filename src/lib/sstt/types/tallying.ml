open Core
open Sstt_utils

type delta = {v : VarSet.t; f : FieldVarSet.t; r : RowVarSet.t}
module type VarSettings = sig
  val tcompare : Var.t -> Var.t -> int
  val fcompare : FieldVar.t -> FieldVar.t -> int
  val rcompare : RowVar.t -> RowVar.t -> int
  val delta : delta
end

(* module type LabelOrder = sig
  val compare : Label.t -> Label.t -> int
end *)

(* module Row = struct
  type t = Records.t * LabelSet.t
end *)

type constr = Ty.t * Ty.t


module Make(VS:VarSettings) = struct

  exception Unsat

  module Var = struct
    include Var
    (* prevent from using default comparison*)
    let equal (_:t) (_:t) = () [@@ocaml.warning "-32"]
    let compare (_:t) (_:t) = () [@@ocaml.warning "-32"]
  end

  module type ConstraintBase = sig
    type var
    module VSet : Set.S with type elt = var
    module T : sig
      type t
      val vars : t -> VSet.t
      val leq : t -> t -> bool
      val cup : t -> t -> t
      val cap : t -> t -> t
      val compare : t -> t -> int
      val empty : t
      val neg : t -> t
      val any : t
    end
    val delta : VSet.t
    val vcompare : var -> var -> int
    type dnf_leaf
    type dnf = (var list * var list * dnf_leaf)
    val to_ty : dnf -> T.t
  end

  module type Constraint = sig
    include ConstraintBase

    (* A constraint is a triple (s ≤ α ≤ t) with lower and upper bounds for
       the variable α *)
    type t = T.t * var * T.t

    (* C1 subsumes C2 if it has the same variable
       and gives better bounds (larger lower bound and smaller upper bound)
    *)
    val subsumes : t -> t -> bool
    val compare : t -> t -> int
    val pos_var : var -> dnf -> t
    val neg_var : var -> dnf -> t
  end

  module MakeConstraint(CB : ConstraintBase) : Constraint
    with type var = CB.var
    and type VSet.t = CB.VSet.t
    and type T.t = CB.T.t
    and type dnf_leaf = CB.dnf_leaf
  = struct
    type var = CB.var
    module VSet = CB.VSet
    module T = CB.T
    type t = T.t * var * T.t

    let delta = CB.delta
    let vcompare = CB.vcompare

    let subsumes (t1, v1, t1') (t2, v2, t2') =
      vcompare v1 v2 = 0 &&
      T.leq t2 t1 && T.leq t1' t2'

    let compare (t1,v1,t1') (t2,v2,t2')=
      vcompare v1 v2 |> ccmp
        T.compare t1 t2 |> ccmp
        T.compare t1' t2'

    type dnf_leaf = CB.dnf_leaf
    type dnf = CB.dnf
    let to_ty = CB.to_ty

    let pos_var v e = (T.empty, v, T.neg (to_ty e))

    let neg_var v e = (to_ty e, v, T.any)
  end

  module TConstr : Constraint
    with type VSet.t = VarSet.t
    and type T.t  = Ty.t
    and type var = Var.t
    and type dnf_leaf = VDescr.Descr.t
  = MakeConstraint(struct
    type var = Var.t
    module VSet = VarSet
    module T = Ty
    let delta = VS.delta.v
    let vcompare v1 v2 = VS.tcompare v1 v2
    type dnf_leaf = VDescr.Descr.t
    type dnf = (var list * var list * VDescr.Descr.t)
    let to_ty e = [ e ] |> VDescr.of_dnf |> T.of_def
  end)

  module FConstr : Constraint
    with type VSet.t = FieldVarSet.t
    and type T.t  = Ty.F.t
    and type var = FieldVar.t
    and type dnf_leaf = Ty.F.dnf_leaf
  = MakeConstraint(struct
    type var = FieldVar.t
    module VSet = FieldVarSet
    module T = struct 
      include Ty.F
      let vars = fvars
    end
    let delta = VS.delta.f
    let vcompare v1 v2 = VS.fcompare v1 v2
    type dnf_leaf = T.dnf_leaf
    type dnf = (var list * var list * dnf_leaf)
    let to_ty e = [ e ] |> T.of_dnf
  end)

  (* A "row-variable atom" is one with empty bindings and tail = RowVar ρ. *)
  let is_row_var_atom (a : Records.Atom.t) =
    LabelMap.bindings a.bindings |> List.is_empty &&
    match a.tail with Records.Tail.RowVar _ -> true | _ -> false

  let get_row_var_atom (a : Records.Atom.t) =
    match a.tail with Records.Tail.RowVar v -> v | _ -> assert false

  let mk_row_var_atom (v : RowVar.t) : Records.Atom.t =
    { bindings = LabelMap.empty; tail = Records.Tail.RowVar v }

  (* Separate the row-variable atoms in a DNF line *)
  let separate_row_vars (ps, ns) =
    let var_ps, rest_ps = List.partition is_row_var_atom ps in
    let var_ns, rest_ns = List.partition is_row_var_atom ns in
    let pvs = List.map get_row_var_atom var_ps in
    let nvs = List.map get_row_var_atom var_ns in
    (pvs, nvs, (rest_ps, rest_ns))
[@@ocaml.warning "-32"]

  (* Row variable extraction from Records.t for vars function *)
  let rvars_of_records (r : Records.t) : RowVarSet.t =
    let add_rvars acc atoms =
      List.fold_left (fun acc (a : Records.Atom.t) ->
        match a.tail with
        | Records.Tail.RowVar v -> RowVarSet.add v acc
        | _ -> acc) acc atoms
    in
    r |> Records.dnf |> List.fold_left (fun acc (ps, ns) ->
      add_rvars (add_rvars acc ps) ns
    ) RowVarSet.empty

  (* Row constraints using MakeConstraint *)
  module RConstr : Constraint
    with type VSet.t = RowVarSet.t
    and type T.t = Records.t
    and type var = RowVar.t
    and type dnf_leaf = (Records.Atom.t list * Records.Atom.t list)
  = MakeConstraint(struct
    type var = RowVar.t
    module VSet = RowVarSet
    module T = struct
      type t = Records.t
      let vars = rvars_of_records
      let leq = Records.leq
      let cup = Records.cup
      let cap = Records.cap
      let neg = Records.neg
      let compare = Records.compare
      let empty = Records.empty
      let any = Records.any
    end
    let delta = VS.delta.r
    let vcompare v1 v2 = VS.rcompare v1 v2
    type dnf_leaf = (Records.Atom.t list * Records.Atom.t list)
    type dnf = (var list * var list * dnf_leaf)
    let to_ty (pvs, nvs, (rest_ps, rest_ns)) =
      let var_atoms_pos = List.map mk_row_var_atom pvs in
      let var_atoms_neg = List.map mk_row_var_atom nvs in
      Records.of_dnf [(var_atoms_pos @ rest_ps, var_atoms_neg @ rest_ns)]
  end)

  (* As in CDuce, we follow POPL'15 but keep constraint merged:
     - A Constraint Set C, is a (sorted list of triples) (s, α, t)
     - Adding a new constraint for an existing variable merges them.
     - If such a constraint is trivially unsatifiable (because s and
       t are both monomorphic and s ≤ t does not hold), then
       a failure is returned
  *)
  module CS(C: Constraint) = struct

    type t = [] | (::) of C.t * t
    let _of_list l =
      List.fold_left (fun acc c -> (c :: acc)) [] l

    let assert_sat s t =
      if C.VSet.subset (C.T.vars s) C.delta &&
         C.VSet.subset (C.T.vars t) C.delta &&
         not (C.T.leq s t)
      then raise_notrace Unsat
    let any = []
    let is_any t = (t = [])
    let singleton ((s, _, t) as e) = assert_sat s t; [e]
    let merge (s, v, t) (s', _, t') =
      let ss = C.T.cup s s' in
      let tt = C.T.cap t t' in
      assert_sat ss tt;
      (ss, v, tt)
    
    let vcompare (_, v1, _) (_, v2, _) = C.vcompare v1 v2

    let rec add c l =
      match l with
        [] -> [ c ]
      | c' :: ll ->
        let n = vcompare c c' in
        if n < 0 then c::l
        else if n = 0 then (merge c c')::ll
        else c' :: add c ll

    let rec cap l1 l2 =
      match l1, l2 with
      | [],  _ -> l2
      | _, []  -> l1
      | c1::ll1, c2::ll2 ->
        let n = vcompare c1 c2 in
        if n < 0 then c1 :: cap ll1 l2
        else if n > 0 then c2 :: cap l1 ll2
        else (merge c1 c2)::cap ll1 ll2

    (* A constraint set l1 subsumes a constraint set l2 if
       forall constraint c2 in m2, there exists
       c1 in t1 such that c1 subsumes c2
    *)
    let rec subsumes l1 l2 =
      match l1, l2 with
      | _, [] -> true
      | [], _ -> false
      | c1::ll1, c2::ll2 ->
        let n = vcompare c1 c2 in
        if n < 0 then subsumes ll1 l2
        else if n > 0 then false
        else C.subsumes c1 c2 && subsumes ll1 ll2

    let rec compare l1 l2 =
      match l1, l2 with
        [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | c1 :: ll1, c2 :: ll2 ->
        let c = C.compare c1 c2 in
        if c <> 0 then c else compare ll1 ll2

    let rec to_list_map f = function
        [] -> List.[]
      | e :: ll -> (f e)::to_list_map f ll
  end

  module CSS = struct
    (* Constraint sets are records with each component representing 
       the subset of constraints of a given kind (type, field, and row). They
       represent the conjunction of these constraints. *)

    module TCS = CS(TConstr)
    module FCS = CS(FConstr)
    module RCS = CS(RConstr)
    type constraint_set = {
      types : TCS.t;
      fields : FCS.t;
      rows : RCS.t;
    }

    (* Sets of constraint sets are ordered list of non subsumable elements.
       They represent union of constraints, so we maintain the invariant
       that we don't want to add a constraint set that subsumes an already
       existing one.
    *)
    type t = constraint_set list
    let empty : t = []
    let is_empty = function [] -> true | _ -> false
    let any : t = [{ types = TCS.any; fields = FCS.any; rows = RCS.any }]
    let is_any = function 
      | [{ types; fields; rows }] when TCS.is_any types && FCS.is_any fields && RCS.is_any rows -> true 
      | _ -> false
    let tsingleton e = [{ types = e; fields = []; rows = [] }]
    let fsingleton e = [{ types = []; fields = e; rows = [] }]
    let rsingleton e = [{ types = []; fields = []; rows = e }]
    let tsingle e =
      try tsingleton (TCS.singleton e)
      with Unsat -> empty
    let fsingle e =
      try fsingleton (FCS.singleton e)
      with Unsat -> empty
    let rsingle e =
      try rsingleton (RCS.singleton e)
      with Unsat -> empty
[@@ocaml.warning "-32"]
    let rec insert_aux ({types;fields;rows} as c) l =
      match l with
        [] -> [c]
      | {types=types';fields=fields';rows=rows'} as c' :: ll ->
        let n1 = TCS.compare types types' in
        let n2 = FCS.compare fields fields' in
        let n3 = RCS.compare rows rows' in
        if n1 < 0 then c::l
        else if n1 = 0 then
          if n2 < 0 then c::l
          else if n2 = 0 then
            if n3 < 0 then c::l
            else if n3 = 0 then ll
            else c' :: insert_aux c ll
          else c' :: insert_aux c ll
        else c' :: insert_aux c ll
    let add ({types;fields;rows} as c) l =
      if (List.exists (fun {types=t;_} -> TCS.subsumes types t) l)
        && (List.exists (fun {fields=f;_} -> FCS.subsumes fields f) l)
        && (List.exists (fun {rows=r;_} -> RCS.subsumes rows r) l)
      then l
      else
        List.filter (fun {types=types';fields=fields';rows=rows'} ->
          (TCS.subsumes types' types && FCS.subsumes fields' fields && RCS.subsumes rows' rows) |> not) l
        |> insert_aux c

    let cup t1 t2 = List.fold_left (fun acc cs -> add cs acc) t1 t2
    let cap t1 t2 =
      (cartesian_product t1 t2) |>
      List.fold_left (fun acc (cs1,cs2) ->
        try 
        let r = {
          types = TCS.cap cs1.types cs2.types;
          fields = FCS.cap cs1.fields cs2.fields;
          rows = RCS.cap cs1.rows cs2.rows;
        } in
        add r acc
        with Unsat -> acc)
      empty

    let cup_lazy t1 t2 =
      if is_any t1 then any
      else cup t1 (t2 ())
    let cap_lazy t1 t2 =
      if is_empty t1 then empty
      else cap t1 (t2 ())

    let map_disj f t =
      List.fold_left (fun acc e -> cup_lazy acc (fun () ->  f e)) empty t
    let map_conj f t =
      List.fold_left (fun acc e -> cap_lazy acc (fun () -> f e)) any t
    let to_list l = l
  end

  module Toplevel (C:Constraint) = struct
    let find_min_var =
      let rec aux acc o_min l =
      match l, o_min with
      | [], None -> None
      | [], Some v -> Some (v, acc)
      | v :: ll, _ when C.VSet.mem v C.delta -> aux (v::acc) o_min ll
      | v :: ll, None -> aux acc (Some v) ll
      | v :: ll, Some v_min ->
        if C.vcompare v v_min < 0 then
          aux (v_min::acc) (Some v) ll
        else aux (v :: acc) o_min ll
      in aux [] None

    (* Extract a constraint for the smallest polymorphic (not in delta) top-level variable of a summand *)
    let extract_smallest (pvs, nvs, d) =
      match find_min_var pvs, find_min_var nvs with
        None, None -> None
      | Some (v, rem_pos), None -> Some (C.pos_var v (rem_pos, nvs, d))
      | None, Some (v, rem_neg) -> Some (C.neg_var v (pvs, rem_neg, d))
      | Some (vp, rem_pos), Some (vn, rem_neg) ->
        if C.vcompare vp vn < 0 then
          Some (C.pos_var vp (rem_pos, nvs, d))
        else
          Some (C.neg_var vn (pvs, rem_neg, d))
  end

  module TToplevel = Toplevel(TConstr)
  module FToplevel = Toplevel(FConstr)
  (* module RToplevel = Toplevel(RConstr) *)

  module VDHash = Hashtbl.Make(VDescr)
  let norm_tuple_gen ~any ~conj ~diff ~disjoint ~norm n (ps, ns) =
    (* Same algorithm as for subtyping tuples.
       We define it outside norm below so that its type can be
       generalized and we can apply it to different ~any/~conj/...
    *)
    let ps = mapn (fun () -> List.init n (fun _ -> any)) conj ps in
    let rec psi acc ss ts () =
      let cstr = ss |> CSS.map_disj norm in
      CSS.cup_lazy cstr (fun () ->
        match ts with
          [] -> CSS.empty
        | tt :: ts ->
          if List.exists2 disjoint ss tt then psi acc ss ts ()
          else fold_distribute_comb (fun acc ss ->
              CSS.cap_lazy acc (psi acc ss ts)) diff acc ss tt
      )
    in psi CSS.any ps ns ()
  let norm t =
    let memo = VDHash.create 16 in
    let rec norm_ty t =
      let vd = Ty.def t in
      match VDHash.find_opt memo vd  with
      | Some cstr -> cstr
      | None ->
        VDHash.add memo vd CSS.any;
        let res =
          if Ty.is_empty t then CSS.any
          else if VarSet.subset (Ty.vars t) VS.delta.v then CSS.empty
          else vd |> VDescr.dnf |> CSS.map_conj norm_summand
        in
        VDHash.remove memo vd ; res
    and norm_summand summand =
      match TToplevel.extract_smallest summand with
      | None ->
        let (_,_,d) = summand in
        norm_descr d
      | Some cs -> CSS.tsingle cs
    and norm_descr d =
      let (cs, others) = d |> Descr.components in
      if others then CSS.empty
      else cs |> CSS.map_conj norm_comp
    and norm_comp c =
      let open Descr in
      match c with
      | Enums c -> norm_enums c
      | Arrows c -> norm_arrows c
      | Intervals c -> norm_intervals c
      | Tags c -> norm_tags c
      | Tuples c -> norm_tuples c
      | Records c -> norm_records c
    and norm_enums d =
      match Enums.destruct d with
      | true, [] -> CSS.any
      | _, _ -> CSS.empty
    and norm_intervals d =
      match Intervals.destruct d with
      | [] -> CSS.any
      | _ -> CSS.empty
    and norm_tags tag =
      let (cs, others) = tag |> Tags.components in
      if others then CSS.empty
      else cs |> CSS.map_conj norm_tagcomp
    and norm_tagcomp c =
      let tag = TagComp.tag c in
      c |> TagComp.dnf |> CSS.map_conj (norm_tag tag)      
    and norm_arrows arr =
      arr |> Arrows.dnf |> CSS.map_conj norm_arrow
    and norm_tuples tup =
      let (comps, others) = tup |> Tuples.components in
      if others then CSS.empty
      else comps |> CSS.map_conj norm_tuplecomp
    and norm_tuplecomp tup =
      let n = TupleComp.len tup in
      tup |> TupleComp.dnf |> CSS.map_conj (norm_tuple n)
    and norm_records r =
      r |> Records.dnf |> CSS.map_conj norm_records_directly
    and norm_arrow (ps, ns) =
      let rec psi t1 t2 ps () =
        let cstr = CSS.cup_lazy (norm_ty t1) (fun () -> norm_ty t2) in
        let cstr_rec () = match ps with
            [] -> CSS.empty
          | (s1, s2) :: ps ->
            if Ty.disjoint t1 s1 || Ty.leq t2 s2 then psi t1 t2 ps ()
            else CSS.cap_lazy
              (psi (Ty.diff t1 s1) t2 ps ())
              (psi t1 (Ty.cap t2 s2) ps)
        in
        CSS.cup_lazy cstr cstr_rec
      in
      let norm_single_neg_arrow ps (t1, t2) =
        let cstr_domain = Ty.diff t1 (List.map fst ps |> Ty.disj) |> norm_ty in
        if CSS.is_empty cstr_domain then CSS.empty
        else
          let cstr_struct () =
            if List.is_empty ps then CSS.any else psi t1 (Ty.neg t2) ps () in
          CSS.cap_lazy cstr_domain cstr_struct
      in
      CSS.map_disj (norm_single_neg_arrow ps) ns
    and norm_tuple n line = norm_tuple_gen ~any:Ty.any ~conj:Ty.conj
        ~diff:Ty.diff ~disjoint:Ty.disjoint ~norm:norm_ty n line
    and norm_tag tag line =
      let tys = TagComp.line_emptiness_checks tag line in
      CSS.map_disj norm_ty tys
    (* and norm_record (ps, ns) =
      let line, n = Records.dnf_line_to_tuple (ps, ns) in
      let disjoint s1 s2 =
        let t = Ty.F.cap s1 s2 in
        Ty.F.is_required t && Ty.F.get t |> Ty.is_empty
      in
      norm_tuple_gen ~any:Ty.F.any ~conj:Ty.F.conj
        ~diff:Ty.F.diff ~disjoint ~norm:norm_oty n line *)
    and norm_oty oty =
      (* TODO here using Ty.F.is_empty might lead to an uncatched GetCache effect? *)
      let dnf = Ty.F.dnf oty in
      (* if List.is_empty dnf then CSS.any
      else *)
        if FieldVarSet.subset (Ty.F.fvars oty) VS.delta.f then
        let n, o = Ty.F.destruct oty in
        if o then CSS.empty else norm_ty n
      else dnf |> CSS.map_conj norm_fsummand

    and norm_fsummand summand =
      match FToplevel.extract_smallest summand with
      | None ->
        let (_, _, leaf) = summand in
        begin match leaf with
        | Ty.F.Absent -> CSS.empty
        | Ty.F.Ty t -> norm_ty t
        end
      | Some cs -> CSS.fsingle cs
    and norm_records_directly (ps, ns) : CSS.t =
      (* let print_record fmt r = (Printer.print_ty Printer.empty_params) fmt (Ty.mk_descr (Descr.mk_record r)) in *)
      let rec psi r0 rvs ns : CSS.t =
        let open Records.Atom in
        let check_field r ns_rest l ty =
          let ty_r = Records.Atom.find l r in
          CSS.cup
          (norm_oty (Ty.F.diff ty ty_r))
          (let updated_bindings =
            LabelMap.add l (Ty.F.cap ty (Ty.F.neg ty_r)) r0.bindings
          in
          psi {r0 with bindings = updated_bindings} rvs ns_rest)
        in
        match ns with
        | [] ->
          CSS.empty
        | r::ns_rest ->
          begin match r.Records.Atom.tail with
          | Open ->
              (* LabelMap.for_all (check_field r ns_rest) r0.bindings *)
              LabelMap.fold (fun acc lbl fld -> CSS.cap (check_field r ns_rest lbl fld) acc) CSS.any r0.bindings
          | v when Records.Tail.equal v r0.tail ->
            (* Format.printf "Enter vequal case@."; *)
              (* LabelMap.for_all (check_field r ns_rest) r0.bindings *)
              LabelMap.fold (fun acc lbl fld -> CSS.cap (check_field r ns_rest lbl fld) acc) CSS.any r0.bindings
          | RowVar v when RowVarSet.mem v rvs ->
            (* Format.printf "Enter vfalse case@."; *)
              (* LabelMap.for_all (check_field r ns_rest) r0.bindings *)
              LabelMap.fold (fun acc lbl fld -> CSS.cap (check_field r ns_rest lbl fld) acc) CSS.any r0.bindings
          | _ ->
            (* Format.printf "Enter other@."; *)
            psi r0 rvs ns_rest
          end
      in
      let normalize_ps ps d =
        let intersect_fields lbl =
          List.fold_left (fun acc a -> Ty.F.cap acc (Records.Atom.find lbl a))
            Ty.F.any ps
        in
        let pos_fields =
          LabelMap.Set.elements d |> List.map (fun lbl -> lbl, intersect_fields lbl)
          |> LabelMap.of_list
        in
        let rowvars, r_tail =
          List.fold_left
          (fun (rvs, t) r ->
            match r.Records.Atom.tail with
            | Open -> rvs, t
            | Closed -> rvs, Records.Tail.Closed
            | RowVar v -> RowVarSet.add v rvs, t)
          (RowVarSet.empty, Open)
          ps
        in
        {Records.Atom.bindings=pos_fields; tail=r_tail}, rowvars
    in
    let dom = List.fold_left
      (fun acc a -> LabelMap.Set.union acc (Records.Atom.dom a))
        LabelMap.Set.empty (ps @ ns)
    in
    let r0, rvs = normalize_ps ps dom in
      (* (LabelMap.exists (fun _ y -> Ty.F.is_empty y) r0.bindings)
         ||
         psi r0 rvs ns *)
      CSS.cup
      (LabelMap.fold (fun acc _lbl fld ->
      CSS.cup acc (norm_oty fld))
      CSS.empty
      r0.bindings)
     (psi r0 rvs ns)
    in
    (*  and norm_row *)
    norm_ty t

  let propagate cs =
    let memo_ty = VDHash.create 8 in
    let rec aux prev (cs : CSS.TCS.t) =
      match cs with
      | CSS.TCS.[] -> prev |> CSS.tsingleton
      | ((t',_, t) as constr) :: cs' ->
        let ty = Ty.diff t' t in
        if VDHash.mem memo_ty (Ty.def ty) then
          aux (CSS.TCS.add constr prev) cs'
        else
          let () = VDHash.add memo_ty (Ty.def ty) () in
          let css = norm ty in
          let css' () = cs |> CSS.TCS.cap prev |> CSS.tsingleton in
          let css = CSS.cap_lazy css css' in
          let res = css |> CSS.to_list |> CSS.map_disj (fun {CSS.types;_} -> (aux CSS.TCS.any types))  in
          VDHash.remove memo_ty (Ty.def ty); res
    in
    aux CSS.TCS.any cs

  let solve {CSS.types = cs; _} =
    let renaming = ref Subst.identity in
    let to_eq (ty1, v, ty2) =
      let v' = Var.mk (Var.name v) in
      renaming := Subst.add v' (Ty.mk_var v) !renaming ;
      (v, Ty.cap (Ty.cup ty1 (Ty.mk_var v')) ty2)
    in
    let rec unify eqs =
      match eqs with
      | [] -> Subst.identity
      | (v,ty)::eqs ->
        let (_,ty') = Ty.of_eqs [v, ty] |> List.hd in
        let s = Subst.singleton v ty' in
        let eqs' = eqs |> List.map (fun (v,eq) -> (v, Subst.apply s eq)) in
        let res = unify eqs' in
        Subst.add v (Subst.apply res ty') res
    in
    cs |> CSS.TCS.to_list_map to_eq |> unify |> Subst.map (Subst.apply !renaming)

  let tally cs =
    let ncss = cs
      |> CSS.map_conj (fun (s,t) -> norm (Ty.diff s t)) in
    let mcss = ncss
      |> CSS.to_list |> CSS.map_disj (fun {CSS.types;_} -> propagate types) in
    mcss |> CSS.to_list |> List.map solve
end

let tally_with_order vs =
  let module Tallying = Make(val vs : VarSettings) in
  Tallying.tally

let tally delta cs =
    let vs = (module struct
    let tcompare = Var.compare
    let fcompare = FieldVar.compare
    let rcompare = RowVar.compare
    let delta = {v = delta; f = FieldVarSet.empty; r = RowVarSet.empty}
  end : VarSettings) in
  tally_with_order vs cs

let tally_with_priority preserve delta =
  let cnt = ref 0 in
  let pmap = List.fold_left
      (fun acc v -> cnt := !cnt + 1 ; VarMap.add v !cnt acc)
      VarMap.empty preserve
  in
  let cmp = (module struct
    let tcompare v1 v2 =
    match VarMap.find_opt v1 pmap, VarMap.find_opt v2 pmap with
    | None, None -> Var.compare v1 v2
    | Some _, None -> 1
    | None, Some _ -> -1
    | Some i1, Some i2 -> compare i2 i1
    let fcompare = FieldVar.compare
    let rcompare = RowVar.compare
    let delta = {v = delta; f = FieldVarSet.empty; r = RowVarSet.empty}
  end : VarSettings)
  in
  tally_with_order cmp

let decompose delta s1 s2 =
  let vars1 = VarSet.union (Subst.domain s1) (Subst.intro s1) in
  let vars2 = VarSet.union (Subst.domain s2) (Subst.intro s2) in
  let vars = VarSet.union vars1 vars2 in
  let fresh, fresh_inv = Subst.refresh (VarSet.diff vars delta) in
  let fresh_vars = Subst.intro fresh in
  let s2 = Subst.compose fresh s2 in
  let cs = VarSet.elements vars |> List.concat_map (fun v ->
      let t1, t2 = Subst.find s1 v, Subst.find s2 v in
      [ t1, t2 ; t2, t1 ]
    )
  in
  tally (VarSet.union delta fresh_vars) cs |> List.map
    (fun s -> Subst.compose fresh_inv s |> Subst.restrict vars)
