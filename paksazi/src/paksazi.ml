(**********************************************************************)
(*                                                                    *)
(*                           ScicosLab                                *)
(*                                                                    *)
(*          Ramine Nikoukhah, INRIA Rocquencourt                      *)
(*          Pierre Weis, INRIA Rocquencourt                           *)
(*                                                                    *)
(*  Copyright 2008-2011,                                              *)
(*  Institut National de Recherche en Informatique et en Automatique. *)
(*  All rights reserved.                                              *)
(*                                                                    *)
(*  This file is distributed under the terms of the ???????? License. *)
(*                                                                    *)
(**********************************************************************)

(* $Id$ *)

open Scanf;;
open Printf;;

type blks = { typ: string;
 insz: int;
 outsz: int;
 clkinsz: int;
 clkoutsz: int;
 depu_: int array;
}
;;

type data = {
  mutable blks: blks list;
  mutable connect: int array list;
  mutable clkconnect: int array list;
}
;;

let paksazi_close_out oc =
  (*Pervasives.*)flush oc;
  (*Pervasives.*)close_out oc
;;

let read_blk ib _blk =
  let res = ref [] in
  let rct = ref true in
  while !rct do
    try
      ignore
        (
         bscanf ib " %d %d %d %d %s "
           (fun a b c d s ->
             let vec = Array.init a (fun _i -> bscanf ib " %d " (fun x -> x)) in
             res :=
               { typ = s; insz = a; outsz = b; clkinsz = c; clkoutsz = d;
                 depu_ = vec; } :: !res ))
    with
    | Scanf.Scan_failure _ib ->
        rct := false;
  done;
  List.rev !res
;;

(*
let rec read_conn ib conn =
  try
    bscanf ib " %d %d %d %d "
      (fun a b c d ->
        [| a; b; c; d; |] ::
        read_conn ib conn)
  with
  | Scanf.Scan_failure _ib -> List.rev conn
;;
*)

let read_conn ib _conn =
  let res = ref [] in
  let rct = ref true in
  while !rct do
    try
      ignore
        (bscanf ib " %d %d %d %d "
           (fun a b c d ->
             res := [| a; b; c; d; |] :: !res ))
    with
    | Scanf.Scan_failure _ib ->
        rct := false;
  done;
  List.rev !res
;;

let rec read_data ib data =
  bscanf ib " %s "
    (function
      | "BLOCKS" -> data.blks <- read_blk ib []; read_data ib data
      | "CONNECT" -> data.connect <- read_conn ib []; read_data ib data
      | "CLKCONNECT" -> data.clkconnect <- read_conn ib []; read_data ib data
      | "END" -> read_data ib data
      | _ -> data
    )
;;

exception AlgebraicLoop;;

type port = {blk: int; prt: int; };;

module Orderedports =
  struct
    type t = port
    let compare x y =
      if x.blk < y.blk then -1
      else if x.blk > y.blk then 1
      else if (x.prt) < (y.prt) then -1
      else if (x.prt) > (y.prt) then 1
      else 0
  end
;;

module SetOrderedports : Set.S with type elt = Orderedports.t =
  Set.Make (Orderedports)
;;

module Orderedblocks = struct
  type t = int
  let compare x y = y - x
end
;;

module SetOrderedblocks : Set.S with type elt = Orderedblocks.t
 = Set.Make (Orderedblocks)
;;

type block= {
  block_typ : string;
  number : int;
  orig_number : int;
  mutable reg_ins : SetOrderedports.t array;
  mutable reg_outs : SetOrderedports.t array;
  mutable ev_ins : SetOrderedports.t array;
  mutable ev_outs : SetOrderedports.t array;
  depu : int array;
  mutable typx : bool;
  mutable typz : bool;
  inher : bool;
  const : bool;
  mutable continuous : bool;
  mutable critical : bool;
  mutable is_primary : int;
  mutable is_active : int;
  mutable weak_prev_nodes : block list;
  mutable strong_prev_nodes : block list;
  mutable root : block;
  mutable index : int;
  mutable in_component : bool;
  mutable not_visited : bool;
}
;;

let make_port i j = { blk = i; prt = j; };;

let create_inports insz connmat i =
  let h i k l v =
    match v with
    | [| x; y; m; n; |] when m = i && n = k ->
      SetOrderedports.add (make_port x y) l
    | _ -> l in

  let rec create_entry k connmat i ll =
    match k with
    | -1 -> ll
    | _ ->
        List.fold_left (h i k) SetOrderedports.empty connmat ::
        create_entry (k - 1) connmat i ll in
  let res = create_entry insz connmat i [] in
  res
;;

let create_outports outsz connmat i =
  let g i j l v =
    match v with
    | [| x; y; m; n; |] when x = i && y = j ->
      SetOrderedports.add (make_port m n) l
    | _ -> l in

  let rec create_entry j connmat i ll =
    match j with
    | -1 -> ll
    | _ ->
      List.fold_left  (g i j) SetOrderedports.empty connmat ::
      create_entry (j - 1) connmat i ll in
  let res = create_entry outsz connmat i [] in
  res
;;

let is_0activated clkconnect i =
  let rec f ll =
    match ll with
    | [] -> false
    | x :: l ->
      (match x with
       | [| _a; _b; c; d; |] when d = 0 && c = i -> true
       | _ -> f l) in
   f clkconnect
;;

let create_clkinports b clkconnect i =
  match b.clkinsz with
  | 0 when not (is_0activated clkconnect i) ->
    Array.init (b.insz + 1) (fun _x -> SetOrderedports.empty)
  | _ ->
    Array.of_list (List.rev (create_inports b.clkinsz clkconnect i))
;;

let blkinit =
  let rec block = {
    number = -1;
    orig_number = -1;
    block_typ = "c";
    reg_ins = Array.of_list [ SetOrderedports.empty; ];
    reg_outs = Array.of_list [ SetOrderedports.empty; ];
    ev_ins = Array.of_list [ SetOrderedports.empty; ];
    ev_outs =
    Array.of_list [ SetOrderedports.empty; SetOrderedports.empty; ];
    depu = [||];
    inher = false;
    typx = false;
    typz = false;
    const = false;
    continuous = false;
    critical = false;
    is_primary = -1;
    is_active = -1;
    weak_prev_nodes = [];
    strong_prev_nodes = [];
    root = block;
    index = 0;
    in_component = false;
    not_visited = true;
  } in
  block
;;

let init_blk_tbl data blk_tbl =

  let rec constr_blk_tbl connect clkconnect blks i =
    prerr_endline (Printf.sprintf "booo %d\n" i);
    match blks with
    | [] -> ()
    | b :: ll ->
      let rec block = {
        number = i;
        orig_number = i;
        block_typ= (if b.typ="lz" then "l" else b.typ);
        reg_ins = Array.of_list ( List.rev (create_inports b.insz connect i));
        reg_outs = Array.of_list( List.rev (create_outports b.outsz connect i));
        ev_ins = create_clkinports b clkconnect i;
        ev_outs = Array.of_list( List.rev (create_outports b.clkoutsz clkconnect i));
        depu = Array.sub b.depu_ 0 b.insz;
        inher = not (b.insz = 0) && (b.clkinsz = 0) && not (is_0activated clkconnect i);
        typx = b.typ = "x" || b.typ = "xz";
        typz = b.typ = "lz"|| b.typ = "z" || b.typ = "xz";
        const = (b.insz = 0 && b.clkinsz = 0) || is_0activated clkconnect i;
        continuous = false;
        critical = false;
        is_primary = -1;
        is_active = -1;
        weak_prev_nodes = [];
        strong_prev_nodes = [];
        root = block;
        index = 0;
        in_component = false;
        not_visited = true;
      } in
      Hashtbl.add blk_tbl i block;
      constr_blk_tbl connect clkconnect ll (i + 1) in

  Hashtbl.add blk_tbl (-1) blkinit;
  constr_blk_tbl data.connect data.clkconnect data.blks 0
;;


let rec add_ports bnum n l =
  match n with
  | 0 -> l
  | m ->
    if List.filter (fun x -> x.blk = bnum && x.prt = m) l = []
    then add_ports bnum (m - 1) ({ blk = bnum; prt = m; } :: l)
    else add_ports bnum (m - 1) l
;;

let get_allclocks blk_tbl =
  let f block_num blocki l =
    add_ports block_num ((Array.length blocki.ev_outs) - 1) l in
  List.rev (Hashtbl.fold f blk_tbl [])
;;


let get_clocks blk_tbl =
  let f block_num blocki l =
    match blocki.block_typ with
    | "l" -> l
    | _ -> add_ports block_num ((Array.length blocki.ev_outs) - 1) l in
  List.rev (Hashtbl.fold f blk_tbl [])
;;

let add_evouts blk_tbl port portset =
 (Hashtbl.find blk_tbl port.blk).ev_outs.(port.prt) <-
    SetOrderedports.union
      (Hashtbl.find blk_tbl port.blk).ev_outs.(port.prt) portset
;;

let add_evins blk_tbl portset porti =
  let f blk_tbl porti port =
    (Hashtbl.find blk_tbl port.blk).ev_ins.(port.prt) <-
       SetOrderedports.add porti
         (Hashtbl.find blk_tbl port.blk).ev_ins.(port.prt) in
  SetOrderedports.iter (f blk_tbl porti) portset
;;

let print_portset portset =
  let print_port port l =
    sprintf "(%d,%d)" port.blk port.prt :: l in
  "{" ^ String.concat "," (SetOrderedports.fold print_port portset []) ^ "}"
;;

let print_blockset blockset =
  let print_block block l = sprintf "%d" block :: l in
  "{" ^ String.concat "," (SetOrderedblocks.fold print_block blockset []) ^ "}"
;;


let remove_link b1 i1 b2 i2 =
  (* printf "removing (%d,%d,%d,%d)\n" b1.number i1 b2.number i2; *)
   b1.ev_outs.(i1) <-
     SetOrderedports.remove { blk = b2.number; prt = i2; } b1.ev_outs.(i1);
   b2.ev_ins.(i2) <-
     SetOrderedports.remove { blk = b1.number; prt = i1; } b2.ev_ins.(i2)
;;

let add_link b1 i1 b2 i2=
  (* printf "adding (%d,%d,%d,%d)\n" b1.number i1 b2.number i2; *)
   b1.ev_outs.(i1) <-
     SetOrderedports.add { blk = b2.number; prt = i2; } b1.ev_outs.(i1);
   b2.ev_ins.(i2) <-
     SetOrderedports.add { blk = b1.number; prt = i1; } b2.ev_ins.(i2);;

let add_reglink b1 i1 b2 i2=
  (* printf "adding reg (%d,%d,%d,%d)\n" b1.number i1 b2.number i2; *)
  b1.reg_outs.(i1) <-
    SetOrderedports.add { blk = b2.number; prt = i2; } b1.reg_outs.(i1);
  b2.reg_ins.(i2) <-
    SetOrderedports.add { blk = b1.number; prt = i1; } b2.reg_ins.(i2)
;;

let get_activating_ports block =
  let union = ref SetOrderedports.empty in
    for i = 0 to Array.length block.ev_ins - 1 do
      union := SetOrderedports.union !union block.ev_ins.(i)
    done;
  !union
;;

let prtset_to_blkset portset =
  let f port set = SetOrderedblocks.add port.blk set in
  SetOrderedports.fold f portset SetOrderedblocks.empty
;;

let get_activating_blocks block =
  prtset_to_blkset (get_activating_ports block)
;;

let comp_iord blk_tbl =
  let f blk block =
    if block.const && not (blk < 1) then
      add_link (Hashtbl.find blk_tbl (-1)) 1 block 0 in
  Hashtbl.iter f blk_tbl
;;

let rec pak_ersi blk_tbl =
  let finished = ref true in
  let f _blk block =
    if block.inher then (
    for i = 1 to Array.length block.reg_ins - 1 do
      let all_act_ports =
        SetOrderedports.fold
          (fun p s ->
           SetOrderedports.union
            (get_activating_ports (Hashtbl.find blk_tbl p.blk)) s)
          block.reg_ins.(i) SetOrderedports.empty in
      let oldsize = ref (SetOrderedports.cardinal block.ev_ins.(i)) in
      SetOrderedports.iter
        (fun p ->
         add_link (Hashtbl.find blk_tbl p.blk) p.prt block i)
        all_act_ports;
      let newsize = SetOrderedports.cardinal block.ev_ins.(i) in
      if not (!oldsize = newsize) then finished := false
    done) in
  Hashtbl.iter f blk_tbl;
  if not !finished then pak_ersi blk_tbl
;;

(* ^ should be used with care
 * a print function should print
 *)

(*
let print_list_ports l str =
  let txt = ref "=[" in
  List.iter
    (fun p ->
     txt := !txt ^ "\n" ^ string_of_int p.blk ^ "," ^ string_of_int p.prt)
    l;
  str ^ !txt ^ "]\n"
;;
*)

let print_list_ports oc l str =
  let print_ports oc l =
    List.iter
      (fun {blk = b; prt = p;} ->
         Printf.fprintf oc "\n%d,%d" b p)
      l in
  Printf.fprintf oc "%s=[%a]\n"
    str print_ports l
;;

let print_list_blocks l str =
  let txt = ref "=[" in
  List.iter
    (fun b ->
     txt := !txt ^ "\n" ^ string_of_int b)
    l;
  str ^ !txt ^ "]\n"
;;

let sci_print_file_error err_blks err_msg filename =
  let sci_print_error err_blks err_msg =
    (print_list_blocks err_blks "err_blks") ^ "err_msg='" ^ err_msg ^ "' \n" in
  let oc = open_out filename in
  fprintf oc "%s"
    "function [ordptr,ordclk,critical,ztyp_blocks,dup,cord,oord,zord,iord,err_blks,err_msg,ok]=test_comp()\n";
  fprintf oc "%s" (sci_print_error err_blks err_msg);
  fprintf oc "%s"
    "ordptr=[],ordclk=[],critical=[],ztyp_blocks=[],dup=[],cord=[],oord=[],zord=[],iord=[],ok=0";
  fprintf oc "%s" "\nendfunction\n";
  paksazi_close_out oc;
  raise AlgebraicLoop
;;

let test_alg_loop blk_tbl =

  let f port =

    let rec g blk =
      let bl = Hashtbl.find blk_tbl blk in
      match bl.block_typ with
      | "l" ->
        if bl.in_component then (
          let err_blks =
            Hashtbl.fold
              (fun i bl l ->
               if bl.in_component then i :: l else l)
              blk_tbl [] in
          sci_print_file_error err_blks "Algebraic event loop" "mlcos.sci");
        bl.in_component <- true;
        for i = 1 to Array.length bl.ev_outs - 1 do
          SetOrderedports.iter (fun x -> g x.blk) bl.ev_outs.(i)
        done;
        bl.in_component <- false;
      | _ ->
        for i = 1 to Array.length bl.reg_outs - 1 do
          SetOrderedports.iter
            (fun x ->
             let xb = Hashtbl.find blk_tbl x.blk in
             if xb.in_component && xb.block_typ = "l" then (
               let err_blks =
                 Hashtbl.fold
                   (fun i bl l ->
                    if bl.in_component then i :: l else l)
                   blk_tbl [] in
               sci_print_file_error err_blks "Algebraic event loop" "mlcos.sci"))
            bl.reg_outs.(i)
        done in
    SetOrderedblocks.iter g
      (prtset_to_blkset (Hashtbl.find blk_tbl port.blk).ev_outs.(port.prt)) in

  List.iter f (get_clocks blk_tbl)
;;


let find_sblocks blk_tbl =
  let f bnum blocki l =
    match blocki.block_typ with
    | "l" -> SetOrderedblocks.add bnum l
    | _ -> l in
  Hashtbl.fold f blk_tbl SetOrderedblocks.empty
;;

let print_blockinfo blk_tbl block =
  let f x a = x ^ "-" ^ print_portset a in
  sprintf "Block number %d :" block ^
  Array.fold_left f "\n reg_ins: " (Hashtbl.find blk_tbl block).reg_ins ^
  Array.fold_left f "\n reg_outs: " (Hashtbl.find blk_tbl block).reg_outs ^
  Array.fold_left f "\n ev_ins: " (Hashtbl.find blk_tbl block).ev_ins ^
  Array.fold_left f "\n ev_outs: " (Hashtbl.find blk_tbl block).ev_outs ^
  sprintf "\n type: %s \n" (Hashtbl.find blk_tbl block).block_typ
;;

let print_blocks_info blk_tbl filename =
  let oc = open_out filename in
  let g blknum _blk l = print_blockinfo blk_tbl blknum ^ "\n" ^ l in
  fprintf oc "%s" (Hashtbl.fold g blk_tbl "");
  paksazi_close_out oc
;;

let modify_links blk_tbl block inter =
  SetOrderedports.iter
    (fun port ->
     for i = 1 to Array.length block.ev_outs - 1 do
       remove_link block i (Hashtbl.find blk_tbl port.blk) port.prt
     done;
     SetOrderedports.iter
       (fun porti ->
        add_link
          (Hashtbl.find blk_tbl porti.blk) porti.prt
          (Hashtbl.find blk_tbl port.blk) port.prt)
       block.ev_ins.(1))
    inter
;;

let upEvent blk_tbl block =
  let inter = ref SetOrderedports.empty in
  inter := block.ev_outs.(1);
  for i = 2 to Array.length block.ev_outs - 1 do
    inter := SetOrderedports.inter !inter block.ev_outs.(i)
  done;
  modify_links blk_tbl block !inter;
  not (SetOrderedports.is_empty !inter)
;;

let upEvents blk_tbl sblkset not_finished =
  let f blk_tbl sblk not_finished =
    upEvent blk_tbl (Hashtbl.find blk_tbl sblk) || not_finished in
  SetOrderedblocks.fold (f blk_tbl) sblkset not_finished
;;

let upAllEvents blk_tbl =
  let rec f blk_tbl sblkset not_finished =
    if upEvents blk_tbl sblkset not_finished
    then f blk_tbl sblkset false else false in
  ignore (f blk_tbl (find_sblocks blk_tbl) false)
;;

let get_primary blk_tbl port =
  prtset_to_blkset (Hashtbl.find blk_tbl port.blk).ev_outs.(port.prt)
;;

let get_event_activates_ports block =
  let union = ref SetOrderedports.empty in
  for i = 1 to Array.length block.ev_outs - 1 do
    if block.block_typ = "l" then
      union := SetOrderedports.union !union block.ev_outs.(i)
  done;
  !union
;;

let get_event_activates block =
  prtset_to_blkset (get_event_activates_ports block)
;;

let block_initialise blk_tbl _blocknum block =
  let depend_on =
    let union = ref SetOrderedblocks.empty in
    for i = 1 to Array.length block.reg_ins - 1 do
      if block.depu.(i - 1) = 1 then
        union :=
          SetOrderedblocks.union !union (prtset_to_blkset block.reg_ins.(i))
    done;
    !union

   and event_activates = get_event_activates block
   and event_depend_on =
     let union = ref SetOrderedblocks.empty in
     for i = 0 to Array.length block.ev_ins - 1 do
       let synchrolinks =
         SetOrderedports.filter
            (fun x ->
             (Hashtbl.find blk_tbl x.blk).block_typ = "l")
            block.ev_ins.(i) in
       union := SetOrderedblocks.union !union (prtset_to_blkset synchrolinks)
     done;
     !union in

   block.weak_prev_nodes <-
     SetOrderedblocks.fold (fun elt s -> Hashtbl.find blk_tbl elt :: s)
       (SetOrderedblocks.union
         (SetOrderedblocks.union depend_on event_depend_on)
         event_activates)
       [];
   block.strong_prev_nodes <-
      SetOrderedblocks.fold (fun elt s -> Hashtbl.find blk_tbl elt :: s)
     (SetOrderedblocks.union depend_on event_depend_on)
     []
;;

let main_initialise blk_tbl =
  Hashtbl.iter (block_initialise blk_tbl) blk_tbl
;;

let constr_prim_list primary blk_tbl =
  let f primaryi l = Hashtbl.find blk_tbl primaryi :: l in
  SetOrderedblocks.fold f primary []
;;

let initialise level primarylist blk_tbl =
  let g primaryi = (* print_int primaryi.number; *)
    primaryi.is_primary <- !level in

  let rec h block =
     block.is_active <- !level;
     block.index <- 0;
     block.not_visited <- true;
     block.root <- block;
     block.in_component <- false;
     List.iter h (constr_prim_list (get_event_activates block) blk_tbl) in

  List.iter g primarylist;
  List.iter h primarylist;
;;

let min node node' = if node.index < node'.index then node else node';;
let gt node node' = node.index > node'.index;;
let empty stack = !stack = [];;
let push elt stack = stack := elt :: !stack;;
let pop stack =
  match !stack with
  | [] -> failwith "top"
  | node :: nodes -> stack := nodes; node
;;

let top stack = match !stack with
  | [] -> failwith "top"
  | node :: _ -> node
;;


let pre_order blk_tbl primarylist level str =
  level:= !level + 1;
  initialise level primarylist blk_tbl;
  let stack = ref []
  and index = ref 0
  and res_ref = ref [] in

  let rec visit1' current_node level str =
    current_node.index <- !index;
    current_node.not_visited <- false;
    current_node.root <- current_node;
    current_node.in_component <- false;
    index := !index + 1;
    List.iter
      (fun node ->
       if node.is_active = level then (
         if node == current_node then (
           sci_print_file_error [node.number] "Algebraic loop" "mlcos.sci");
         if node.not_visited then visit1' node level str;
         if not node.in_component then
           current_node.root <- min node.root current_node.root
       )
      )
      (if str="weak"
       then current_node.weak_prev_nodes
       else current_node.strong_prev_nodes);
    if current_node.root == current_node then begin
      current_node.in_component <- true;
      let comp_ref = ref [current_node] in
      while not (empty stack) && gt (top stack) current_node do
        let node = pop stack in
        node.in_component <- true;
        if List.mem node primarylist then begin
          comp_ref := node :: !comp_ref
        end;
      done;
      res_ref := !comp_ref :: !res_ref
    end
    else push current_node stack in

  List.iter
    (fun node ->
     if node.not_visited then visit1' node !level str)
    primarylist;
  (*print_ord str (List.rev !res_ref);*)

  List.rev !res_ref
;;

let duplicate bl blk_tbl port =
  let empty_ports _ = SetOrderedports.empty in
  let get_newblock_num blk_tbl =
    let m = ref (-1) in
    Hashtbl.iter (fun bnum _block -> if bnum > !m then m := bnum) blk_tbl;
    !m + 1 in
  let newblk = get_newblock_num blk_tbl in
  let rec newblock = {
     number = newblk;
     orig_number = bl.orig_number;
     block_typ = "l";
     reg_ins = Array.init (Array.length bl.reg_ins) empty_ports;
     reg_outs = Array.init (Array.length bl.reg_outs) empty_ports;
     ev_ins = Array.init (Array.length bl.ev_ins) empty_ports;
     ev_outs = Array.init (Array.length bl.ev_outs) empty_ports;
     depu = [| 1 |];
     inher = bl.inher;
     typx = bl.typx;
     typz = bl.typz;
     const = false;
     continuous = false;
     critical = false;
     is_primary = -1;
     is_active = -1;
     weak_prev_nodes = [];
     strong_prev_nodes = [];
     root = newblock;
     index = 0;
     in_component = false;
     not_visited = true;
  } in
  Hashtbl.add blk_tbl newblk newblock;
  (* printf "adding new block %d duplicating %d\n" newblk bl.number;*)
  SetOrderedports.iter
    (fun p -> add_reglink (Hashtbl.find blk_tbl p.blk) p.prt newblock 1)
    bl.reg_ins.(1);
  for i = 1 to Array.length bl.ev_outs - 1 do
    SetOrderedports.iter
      (fun p -> add_link newblock i (Hashtbl.find blk_tbl p.blk) p.prt)
      bl.ev_outs.(i)
  done;
  remove_link (Hashtbl.find blk_tbl port.blk) port.prt bl 1;
  add_link (Hashtbl.find blk_tbl port.blk) port.prt newblock 1;
  newblock
;;

let new_duplicate bl blk_tbl ports =
  let empty_ports _ = SetOrderedports.empty in
  let get_newblock_num blk_tbl =
    let m = ref (-1) in
    Hashtbl.iter (fun bnum _block -> if bnum > !m then m:=bnum) blk_tbl;
    !m + 1 in
  let newblk = get_newblock_num blk_tbl in
  let rec newblock = {
    number = newblk;
    orig_number = bl.orig_number;
    block_typ = "l";
    reg_ins = Array.init (Array.length bl.reg_ins) empty_ports;
    reg_outs = Array.init (Array.length bl.reg_outs) empty_ports;
    ev_ins = Array.init (Array.length bl.ev_ins) empty_ports;
    ev_outs = Array.init (Array.length bl.ev_outs) empty_ports;
    depu = [| 1 |];
    inher = bl.inher;
    typx = bl.typx;
    typz = bl.typz;
    const = false;
    continuous = false;
    critical = false;
    is_primary = -1;
    is_active = -1;
    weak_prev_nodes = [];
    strong_prev_nodes = [];
    root = newblock;
    index = 0;
    in_component = false;
    not_visited = true;
  } in
  Hashtbl.add blk_tbl newblk newblock;
  (* printf "adding new block %d duplicating %d\n" newblk bl.number;*)
  SetOrderedports.iter
    (fun p -> add_reglink (Hashtbl.find blk_tbl p.blk) p.prt newblock 1)
    bl.reg_ins.(1);
  for i = 1 to Array.length bl.ev_outs - 1 do
    SetOrderedports.iter
      (fun p -> add_link newblock i (Hashtbl.find blk_tbl p.blk) p.prt)
      bl.ev_outs.(i)
  done;
  SetOrderedports.iter
    (fun port ->
     remove_link (Hashtbl.find blk_tbl port.blk) port.prt bl 1;
      add_link (Hashtbl.find blk_tbl port.blk) port.prt newblock 1)
    ports;
  newblock
;;

let rec update_diag bl blk_list blk_tbl port =
   (* printf "pivot %d; list=" bl.number;
  List.iter (fun x -> printf " %d" x.number) blk_list;printf "\n";*)
  if SetOrderedports.cardinal bl.ev_ins.(1) = 1 then (
    let g blockj =
      for j = 0 to Array.length blockj.ev_ins - 1 do
        let bport = Hashtbl.find blk_tbl port.blk in
        if SetOrderedports.mem
             { blk = blockj.number; prt = j; }
             bport.ev_outs.(port.prt)
        then (
          remove_link bport port.prt blockj j;
          for i = 1 to Array.length bl.ev_outs - 1 do
            add_link bl i blockj j;
          done);
      done;
      block_initialise blk_tbl blockj.number blockj in

    List.iter g blk_list;
    block_initialise blk_tbl bl.number bl;
    bl
  )
  else (
    let newblock = duplicate bl blk_tbl port in
    block_initialise blk_tbl bl.number bl;
    block_initialise blk_tbl newblock.number newblock;
    update_diag newblock blk_list blk_tbl port;
  )
;;

let block_list_to_num_list blist =
   List.fold_left (fun l b -> b.number :: l) [] blist
;;

let ordonnance blk_tbl port level =
  let primary = get_primary blk_tbl port in
  let primarylist = constr_prim_list primary blk_tbl in
  let ordered_list = (* printf " port (%d,%d) " port.blk port.prt;*)
    pre_order blk_tbl primarylist level "weak" in
  let f ord x =
    match x with
    | [ xi ] -> xi :: ord
    | _ ->
      let first = ref true and to_move_down = ref [] in
      let k = pre_order blk_tbl x level "strong" in
      let g ord ki =
        match ki with
        | [] -> ord
        | [ y0 ] when !first -> first := false; y0 :: ord
        | [ y0 ] -> to_move_down := y0 :: !to_move_down; ord
        | _ ->
          sci_print_file_error
            (block_list_to_num_list ki)
            "Algebraic loop" "mlcos.sci" in
      let ord = List.fold_left g ord k in
      to_move_down :=
        List.filter
          (fun x ->
           let is_in = ref false in
           for j = 0 to Array.length x.ev_ins - 1 do
             is_in := !is_in || SetOrderedports.mem port x.ev_ins.(j)
           done;
           !is_in)
          !to_move_down;
      (if not (!to_move_down = [])
       then update_diag (List.nth ord 0) !to_move_down blk_tbl port
       else List.nth ord 0) :: List.tl ord in

  let ord = List.fold_left f [] ordered_list
  and g ordclk ordi =
    let ind = ref 1 and nevprt = ref 0 in
      for i = 1 to Array.length ordi.ev_ins - 1 do
        if SetOrderedports.mem port ordi.ev_ins.(i) then
          nevprt := !nevprt + !ind;
        ind := 2 * !ind
      done;
      { blk = ordi.number; prt = !nevprt; } :: ordclk in
  List.fold_left g [] ord
;;

let rec update_todo blk_tbl ordclki todo' =
  match ordclki with
  | [] -> todo'
  | { blk = blk; prt = _nevprt; } :: ll ->
    let blocki = Hashtbl.find blk_tbl blk in
    if blocki.block_typ = "l" then
      update_todo blk_tbl ll
        (add_ports blk (Array.length blocki.ev_outs - 1) todo')
   else update_todo blk_tbl ll todo'
;;

let find_ext_cord blk_tbl =
  let cord = (Hashtbl.find blk_tbl 0).ev_outs.(1) in
  let rec f ext_cord exti =
    if not (SetOrderedports.is_empty exti) then (
      let exti' =
        SetOrderedports.fold
          (fun x a ->
           SetOrderedports.union a
             (get_event_activates_ports (Hashtbl.find blk_tbl x.blk)))
          exti SetOrderedports.empty in
      f (SetOrderedports.union ext_cord exti) exti' )
    else ext_cord in

  let exe_cord' = f SetOrderedports.empty cord in

  let exec_cord_blks =
    SetOrderedports.fold
      (fun port blks -> Hashtbl.find blk_tbl port.blk :: blks)
      exe_cord' []
  and g exe_cord block =
    let ind = ref 1 and nevprt = ref 0 in
    for i = 1 to Array.length block.ev_ins - 1 do
      if SetOrderedports.mem { blk = block.number; prt = i; } exe_cord' then
        nevprt := !nevprt + !ind;
      ind := 2 * !ind
    done;
    SetOrderedports.add { blk = block.number; prt = !nevprt; } exe_cord in

  List.fold_left g  SetOrderedports.empty exec_cord_blks
;;

let block_initialise2 blk_tbl level blocknum =
  let block = Hashtbl.find blk_tbl blocknum in
  let depend_on =
    let union = ref SetOrderedblocks.empty in
    for i = 1 to Array.length block.reg_ins - 1 do
      union :=
        SetOrderedblocks.union !union (prtset_to_blkset block.reg_ins.(i))
    done;
    !union

and event_depend_on =
  let union = ref SetOrderedblocks.empty in
  for i = 0 to Array.length block.ev_ins - 1 do
    let synchrolinks =
      SetOrderedports.filter
        (fun x -> (Hashtbl.find blk_tbl x.blk).block_typ = "l")
        block.ev_ins.(i) in
    union := SetOrderedblocks.union !union (prtset_to_blkset synchrolinks)
  done;
  !union in

  block.strong_prev_nodes <-
    SetOrderedblocks.fold
      (fun elt s -> Hashtbl.find blk_tbl elt :: s)
      (SetOrderedblocks.union depend_on event_depend_on) [];
  block.is_active <- level;
  block.not_visited <- true;
;;

let print_block_set set =
  let ss =
    sprintf "set =" ^
    SetOrderedblocks.fold (fun a x -> x ^ "-" ^ sprintf "(%d)" a)
      set " " in
  printf "%s" ss
;;

let print_port_set set =
  let ss =
    sprintf "\nportset =" ^
    SetOrderedports.fold (fun a x -> x ^ "-" ^ sprintf "(%d,%d)" a.blk a.prt)
      set " " in
  printf "%s" ss
;;

let get_ozord blk_tbl ext_cord ordclk level =
  (* print_block_set ext_cord;*)
  let cord = Hashtbl.find ordclk { blk = 0; prt = 1; } in
  (* List.iter (fun p -> printf "cordi=%d " p.blk) cord; *)
  level:= !level + 1;
  SetOrderedblocks.iter (block_initialise2 blk_tbl !level) ext_cord;
  let rec f block =
    block.is_primary <- !level;
    (* printf "\n l=%d \n" !level;print_int block.number; *)
    block.not_visited <- false;
    List.iter
      (fun blocki ->
       (* printf " l=%d block=%d " !level block.number;print_int blocki.number;
          printf " blocki.is_active=%d \n" blocki.is_active;*)
       if blocki.is_active = !level && blocki.not_visited then f blocki)
      block.strong_prev_nodes in
  SetOrderedblocks.iter
    (fun x ->
     let block = Hashtbl.find blk_tbl x in
     if block.typx then f block)
    ext_cord;

  let oord =
    List.filter
      (fun x ->
       (* printf "ispri=%d  level=%d" (Hashtbl.find blk_tbl x.blk).is_primary !level;*)
       (Hashtbl.find blk_tbl x.blk).is_primary = !level)
      cord in
  level := !level+1;
  SetOrderedblocks.iter (block_initialise2 blk_tbl !level) ext_cord;
  SetOrderedblocks.iter
    (fun x ->
     let block = Hashtbl.find blk_tbl x in
     if block.typz then f block)
    ext_cord;
  let zord =
    List.filter
      (fun x ->
       (Hashtbl.find blk_tbl x.blk).is_primary = !level)
    cord in
  (* List.iter (fun p-> printf "ordi=%d " p.blk) oord; *)
  (oord, zord)
;;

let print_todo l =
  printf "\n todo =";
  List.iter (fun p -> printf "(%d, %d)" p.blk p.prt) l;
  printf "\n"
;;

let paksazi blk_tbl =
  Hashtbl.iter (block_initialise blk_tbl) blk_tbl;
  let level = ref 1 in
  let ordclk = Hashtbl.create 10000 in
  let todo = get_clocks blk_tbl in
  let rec pak todo = (* print_todo todo; *)
    match todo with
    | [] -> ordclk
    | port :: todo' ->
      if not (Hashtbl.mem ordclk port) then (
        let ordclki = ordonnance blk_tbl port level in
        Hashtbl.add ordclk port ordclki;
        let new_todo = update_todo blk_tbl ordclki todo' in
        pak new_todo)
      else pak todo' in

  let ordclk = pak todo
  and ext_cord = find_ext_cord blk_tbl in
  let ext_cord_blk = prtset_to_blkset ext_cord in
  let (oord,zord) = get_ozord blk_tbl ext_cord_blk ordclk level in
  let iscovered p1 p2 = p1.blk = p2.blk && (p1.prt land p2.prt) = p1.prt in
  Hashtbl.iter
    (fun i ordclki ->
     match i.blk with
     | -1 | 0 -> ()
     | _ ->
       (* print_port_set ext_cord; printf "\nordclki %s"
            (print_list_ports ordclki ""); *)
       let ordclki' = List.rev
           (
            List.fold_left
              (fun b ai ->
                if SetOrderedports.exists (iscovered ai) ext_cord
                then { blk = ai.blk; prt = -(ai.prt); } :: b
                else ai :: b)
              [] ordclki ) in
       Hashtbl.replace ordclk i ordclki')
    ordclk;
  Hashtbl.add ordclk { blk = (-2); prt = 1; } oord;
  Hashtbl.add ordclk { blk = (-3); prt = 1; } zord;
  SetOrderedblocks.iter
    (fun i -> (Hashtbl.find blk_tbl i).continuous <- true)
    ext_cord_blk;
  Hashtbl.iter
    (fun _i b -> if not b.continuous then (b.typx <- false; b.typz <- false))
    blk_tbl;
  ordclk
;;

let is_critical blk_tbl _critev _porti ordclki =
  let rec f ordclki =
    match ordclki with
    | [] -> false
    | p :: ordclki' ->
      (Hashtbl.find blk_tbl p.blk).critical && p.prt > 0 || f ordclki' in
  f ordclki
;;

let get_critev blk_tbl ordclk =
  List.iter
    (fun x -> (Hashtbl.find blk_tbl x.blk).critical <- true)
    (Hashtbl.find ordclk { blk = (-2); prt = 1; });
  List.iter
    (fun x -> (Hashtbl.find blk_tbl x.blk).critical <- true)
    (Hashtbl.find ordclk { blk = (-3); prt = 1; });
  let depend_on_block _i block set =
    if block.critical then (
      let union = ref SetOrderedblocks.empty in
      for i = 1 to Array.length block.reg_ins - 1 do
         union :=
           SetOrderedblocks.union !union (prtset_to_blkset block.reg_ins.(i))
      done;
      SetOrderedblocks.union !union set
      ) else set in

  SetOrderedblocks.iter
     (fun b -> (Hashtbl.find blk_tbl b).critical <- true)
     (Hashtbl.fold depend_on_block blk_tbl SetOrderedblocks.empty);
  let critev = Hashtbl.create 10000 in
  Hashtbl.iter
    (fun porti ordclki ->
     if porti.blk > 0 then
       if is_critical blk_tbl critev porti ordclki
       then Hashtbl.add critev porti true
       else Hashtbl.add critev porti false)
    ordclk;
  (* printf "\n Criticial events are: "; Hashtbl.iter (fun p c ->
       if c then (printf "(%d,%d) " p.blk p.prt)) critev; *)
  critev
;;

let print_ordclki ordclki port =
  sprintf "port [%d,%d] :" port.blk port.prt ^
  List.fold_left (fun x a -> x ^ " " ^ sprintf "(%d,%d)" a.blk a.prt) " " ordclki
;;

let print_critev critev =
  sprintf "Critical events: " ^
  Hashtbl.fold
    (fun x t a ->
     if t then a ^ " " ^ sprintf "(%d,%d)" x.blk x.prt else a)
    critev " "
;;

let print_typz blk_tbl =
  sprintf "\n Ztype blocks: " ^
  Hashtbl.fold
    (fun i b a ->
     if b.typz then a ^ " " ^ sprintf " %d " i else a)
    blk_tbl " "
;;

let print_duplicate blk_tbl =
  sprintf "\n Duplicate blocks: \n" ^
  Hashtbl.fold
    (fun _i b a ->
     if b.number > 0 then
       a ^ " " ^ sprintf " %d .. %d\n" b.number b.orig_number else a)
    blk_tbl " "
;;
(*
let get_event_number blk_tbl =
  let event_number = Hashtbl.create 100 in
  let k = ref 1 in
  Hashtbl.iter
    (fun n b ->
     if n > 0 then
       for i = 1 to Array.length b.ev_outs - 1 do
         Hashtbl.add event_number { blk = n; prt = i; } !k;
         k := !k +1;
       done)
    blk_tbl;
  event_number
;;
*)
let get_event_number blk_tbl =
  let blocks = Array.make (Hashtbl.length blk_tbl) 0 in
     Hashtbl.iter
       (fun n b ->  if n > 0 then (
          blocks.(n) <- Array.length b.ev_outs)
       )
    blk_tbl;
  let event_numbers = Hashtbl.create 100 in
    let k = ref 1 in
       Array.iteri
         (fun n nports ->
           for i = 1 to nports - 1 do
             Hashtbl.add event_numbers { blk = n; prt = i; } !k;
             k := !k + 1; done
         )
       blocks;
  event_numbers
;;


let sci_print_ord oc ordclk blk_tbl =

  let print_port oc n i =
    let port = make_port n i in
    if Hashtbl.mem ordclk port then
      let ports = Hashtbl.find ordclk port in
      List.iter
        (fun {blk = b; prt = p; } ->
            Printf.fprintf oc "\n%d,%d" b p)
        ports in

  let print_ptr oc accu n i =
    let port = make_port n i in
    if Hashtbl.mem ordclk port then
      let ports = Hashtbl.find ordclk port in
      Printf.fprintf oc ";%d" (List.length ports + accu);
      (List.length ports + accu)
    else
      (
       Printf.fprintf oc "%d;" accu;
       accu) in

  let print_ordclk_i oc n ev_out =
    let lim = Array.length ev_out - 1 in
    let rec loop i =
      if i <= lim then
        (
         print_port oc n i;
         loop (i + 1)
        ) in
    loop 1 in

  let print_ordptr_i oc accu1 n ev_out =
    let lim = Array.length ev_out - 1 in
    let rec loop i accu =
      if i > lim then accu else
      let accu = print_ptr oc accu n i in
      loop (i + 1) accu in
    loop 1 accu1 in

  let print_ordptr oc () =
    let lim = Hashtbl.length blk_tbl - 1 in
    let rec loop n accu =
      if n >= lim then () else
      let accu =
        if Hashtbl.mem blk_tbl n then
          let b = Hashtbl.find blk_tbl n in
          print_ordptr_i oc accu n b.ev_outs
        else
          accu in
      loop (n + 1) accu in
    loop 1 1 in

  let print_ordclk oc () =
    let lim = Hashtbl.length blk_tbl - 1 in
    let rec loop n  =
      if n >= lim then () else
      begin
        if Hashtbl.mem blk_tbl n then
          begin
            let b = Hashtbl.find blk_tbl n in
            print_ordclk_i oc n b.ev_outs;
          end;
        loop (n + 1)
      end in
    loop 1 in

  Printf.fprintf oc
    "ordptr = [1%a];\n\
     ordclk = [%a];\n"
    print_ordptr ()
    print_ordclk ()
;;
(*

  let ordptr = ref "[1" and counter = ref 1 and ordclkstr = ref "[" in
  let nblk = Hashtbl.length blk_tbl in
  for n=1 to nblk do (
    if Hashtbl.mem blk_tbl n then
      let b = Hashtbl.find blk_tbl n in

      for i = 1 to Array.length b.ev_outs - 1 do
       (*printf "==%d,%d==" n i;*)
       try
         let y = Hashtbl.find ordclk { blk = n; prt = i; } in
         counter := !counter + List.length y;
         ordptr := !ordptr ^ ";" ^ string_of_int !counter;
         List.iter
           (fun p ->
            ordclkstr := !ordclkstr ^ "\n" ^
            string_of_int p.blk ^ "," ^ string_of_int p.prt)
           y
        with
        | _ -> ordptr := !ordptr ^ ";" ^ string_of_int !counter
      done;)
   done;
  "ordptr = " ^ !ordptr ^ "]\n ordclk=" ^ !ordclkstr ^ "]"
;;
*)

let sci_print_critev critev event_number =
  let txt =
    sprintf "\n critical= [" ^
    Hashtbl.fold
      (fun x t a ->
       if t then
         (* printf "%d--%d\n" x.blk x.prt; *)
         a ^ " " ^ string_of_int (Hashtbl.find event_number x) else a)
      critev " " in
  txt ^ "]\n"
;;

let sci_print_typz blk_tbl =
  let txt =
    sprintf "\n ztyp_blocks=[ " ^
    Hashtbl.fold
      (fun i b a -> if b.typz then a ^ " " ^ sprintf " %d " i else a)
      blk_tbl " " in
  txt ^ "]\n"
;;

let sci_print_duplicate blk_tbl =
  let txt = sprintf "\n dup=[" ^
  Hashtbl.fold
    (fun _i b a ->
     if b.number > 0
     then a ^ " " ^ sprintf "\n %d,%d" b.number b.orig_number
     else a)
    blk_tbl " " in
  txt ^ "]\n"
;;

let print_sci_file blk_tbl ordclk critev filename =
  let event_number = get_event_number blk_tbl in
  let oc = open_out filename in
  fprintf oc "%s"
    "function [ordptr,ordclk,critical,ztyp_blocks,dup,cord,oord,zord,iord,err_blks,err_msg,ok]=test_comp()\n";
  prerr_endline "pakzazi 9";
  sci_print_ord oc ordclk blk_tbl;
  prerr_endline "pakzazi 9";
  fprintf oc "%s" (sci_print_critev critev event_number);
  prerr_endline "pakzazi 9";
  fprintf oc "%s" (sci_print_typz blk_tbl);
  prerr_endline "pakzazi 9";
  fprintf oc "%s" (sci_print_duplicate blk_tbl);
  prerr_endline "pakzazi 9";
  print_list_ports oc (Hashtbl.find ordclk { blk = 0; prt = 1; }) "cord";
  print_list_ports oc (Hashtbl.find ordclk { blk = (-1); prt = 1; }) "iord";
  print_list_ports oc (Hashtbl.find ordclk { blk = (-2); prt = 1; }) "oord";
  print_list_ports oc (Hashtbl.find ordclk { blk = (-3); prt = 1; }) "zord";
  prerr_endline "pakzazi 9";
  fprintf oc "%s" "ok=1,err_blks=[],err_msg=''\n";
  fprintf oc "%s" "endfunction\n";
  paksazi_close_out oc;
;;

let main () =
  try
    let ib = Scanning.from_file "datacos" in
    let data = read_data ib { blks = []; connect = []; clkconnect = []; } in
     Printf.printf "one";print_endline "";
    let blk_tbl = Hashtbl.create 100000 in
    init_blk_tbl data blk_tbl;
    Printf.printf "two";print_endline "";
    comp_iord blk_tbl;
     Printf.printf "three";print_endline "";
    pak_ersi blk_tbl;
     Printf.printf "four";print_endline "";
    upAllEvents blk_tbl;
     Printf.printf "five";print_endline "";
    test_alg_loop blk_tbl;
     Printf.printf "six";print_endline "";
    let ordclk = paksazi blk_tbl in
     Printf.printf "seven";print_endline "";
    let critev = get_critev blk_tbl ordclk in
     Printf.printf "height";print_endline "";
    print_sci_file blk_tbl ordclk critev "mlcos.sci";
     Printf.printf "end";print_endline "";
  with
  | AlgebraicLoop -> ()
;;

main ()
;;

(*
 Local Variables:
  compile-command: "cd ..; make"
  End:
*)
