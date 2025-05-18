(* Once you are done writing the code, remove this directive,
   whose purpose is to disable several warnings. *)

(* You should read and understand active_borrows.ml *fully*, before filling the holes
  in this file. The analysis in this file follows the same structure. *)

open Type
open Minimir

type analysis_results = label -> PlaceSet.t

let go prog mir : analysis_results =
  (* The set of all places appearing in the MIR code. We are not interesting in initializedness for places
    which are not member of this set. *)
  let all_places =
    let acc =
      Hashtbl.fold
        (fun l _ acc -> PlaceSet.add (PlLocal l) acc)
        mir.mlocals PlaceSet.empty
    in
    Array.fold_left
      (fun acc (i, _) ->
        match i with
        | Ideinit _ | Igoto _ | Ireturn -> acc
        | Iif (pl, _, _) -> PlaceSet.add pl acc
        | Icall (_, pls, pl, _) -> PlaceSet.add_seq (Seq.cons pl (List.to_seq pls)) acc
        | Iassign (pl, rv, _) -> (
            let acc = PlaceSet.add pl acc in
            match rv with
            | RVplace pl | RVborrow (_, pl) | RVunop (_, pl) -> PlaceSet.add pl acc
            | RVbinop (_, pl1, pl2) -> PlaceSet.add pl1 (PlaceSet.add pl2 acc)
            | RVmake (_, pls) -> PlaceSet.add_seq (List.to_seq pls) acc
            | RVunit | RVconst _ -> acc))
      acc mir.minstrs
  in

  (* The set of subplaces of a given place. *)
  let subplaces = Hashtbl.create 7 in
  let () =
    PlaceSet.iter
      (fun pl ->
        let pls = PlaceSet.filter (fun pl_sub -> is_subplace pl_sub pl) all_places in
        Hashtbl.add subplaces pl pls)
      all_places
  in

  (* Effect of initializing a place [pl] on the abstract state [state]: [pl] and all its subplaces
    become initialized. Hence, given that the state is the set of uninitialized places, we remove
    the subplaces [pl] from the abstract state.

    It is important to understand that places can be nested. In order to make a place uninitialized,
    it is *not* enough to remove it from the state. One should also remove subplaces. *)
  let initialize pl state = PlaceSet.diff state (Hashtbl.find subplaces pl) in

  (* This is the dual: we are consuming or deinitiailizing place [pl], so all its subplaces
    become uninitialized, so they are added to [state]. *)
  let deinitialize pl state = PlaceSet.union state (Hashtbl.find subplaces pl) in

  (* Effect of using (copying or moving) a place [pl] on the abstract state [state]. *)
  let move_or_copy pl state =
    let is_copy = typ_is_copy prog (typ_of_place prog mir pl) in 
    if is_copy then
      state
    else
      deinitialize pl state
  in

  (* These modules are parameters of the [Fix.DataFlow.ForIntSegment] functor below. *)
  let module Instrs = struct let n = Array.length mir.minstrs end in
  let module Prop = struct
    type property = PlaceSet.t

    let leq_join p q = if PlaceSet.subset p q then q else PlaceSet.union p q
  end in
  let module Graph = struct
    type variable = int
    type property = PlaceSet.t

    (* To complete this module, one can read file active_borrows.ml, which contains a
      similar data flow analysis. *)

    let foreach_root go =
      let inter = all_places in
      go mir.mentry inter

    let foreach_successor lbl state go =
      let label = fst mir.minstrs.(lbl) in
      match label with
      | Iassign (pl, rvalue, new_label) -> 
            let new_state = 
              match rvalue with 
                | RVplace pll -> move_or_copy pll state
                | RVconst _ -> state
                | RVunit -> state
                | RVborrow (_, pll) -> move_or_copy pll state
                | RVbinop (_, pl1, pl2) -> 
                  (
                    let inter = move_or_copy pl1 state in
                    move_or_copy pl2 inter
                  )
                | RVunop (_, pll) -> move_or_copy pll state
                | RVmake (_, pl_list) -> List.fold_right move_or_copy pl_list state
            in
            let new_state = initialize pl new_state in
            go new_label new_state
      | Ideinit (local, new_label) ->
            let new_state = deinitialize (PlLocal local) state in
            go new_label new_state
      | Igoto new_label -> go new_label state
      | Iif (pl, new_label1, new_label2) ->
            let new_state = move_or_copy pl state in
            go new_label1 new_state;
            go new_label2 new_state
      | Ireturn -> ()
      | Icall (_, pl_list, pl, new_label) ->
            let new_state = List.fold_right move_or_copy pl_list state in
            let new_state2 = initialize pl new_state in
            go new_label new_state2
  end in
  let module Fix = Fix.DataFlow.ForIntSegment (Instrs) (Prop) (Graph) in
  fun i -> Option.value (Fix.solution i) ~default:PlaceSet.empty
