let initialize () =
  let store = Store.create () in
  let gnttab = Gnt.Gnttab.interface_open () in
  let advance_next_frequent_ops () = () in
  let domains_init eventchn =
    Domains.init eventchn gnttab advance_next_frequent_ops
  in
  let doms = domains_init @@ Event.init () in
  let cons = Connections.create () in
  (store, doms, cons)

let create_dom0_conn cons doms =
  (* NOTE: We can't use Domains.create0 since that opens several files
     unavailable in the test env *)
  let dom0 = Domains.create ~remote_port:0 doms 0 1337n in
  Connections.add_domain cons dom0 ;
  Hashtbl.find cons.domains 0

let create_domU_conn cons doms domid =
  let ndom = Domains.create ~remote_port:domid doms domid 1337n in
  Connections.add_domain cons ndom ;
  Hashtbl.find cons.domains domid

let none = Transaction.none

let op_testable =
  Alcotest.testable (Fmt.of_to_string Xenbus.Op.to_string) Stdlib.( = )

let terminate_with_null_char s = Printf.sprintf "%s\000" s

let check_result (reply : Xenbus.Packet.t)
    (expected : Xenbus.Op.operation * string list) =
  let expected_ty, expected_data = expected in
  let expected_data = String.concat "\000" expected_data in
  let expected_data =
    Xenbus.Op.(
      match reply.ty with
      | Read | Directory ->
          expected_data
      | _ ->
          terminate_with_null_char expected_data
    )
  in
  Alcotest.(check' op_testable)
    ~msg:"Verify response type is as expected" ~actual:reply.ty
    ~expected:expected_ty ;
  Alcotest.(check' string)
    ~msg:"Verify payload is as expected" ~actual:reply.data
    ~expected:expected_data

let rpc store cons doms con tid ty payload =
  let data = payload |> String.concat "\000" |> terminate_with_null_char in
  let req = Packet.{tid; rid= 0; ty; data} in
  Process.process_packet ~store ~cons ~doms ~con ~req ;
  Xenbus.Xb.unsafe_pop_output con.xb

let start_transaction store cons doms dom0 =
  (rpc store cons doms dom0 none Transaction_start [""]).data
  |> String.split_on_char '\000'
  |> List.hd
  |> int_of_string

(* Runs a sequence of calls, testing the reply to each one is as expected *)
let run store cons doms payloads =
  List.iter
    (fun (con, tid, (ty, payload), expected_result) ->
      check_result (rpc store cons doms con tid ty payload) expected_result
    )
    payloads

(* Write a path and check the parent nodecan be read *)
let test_implicit_create () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let domU = create_domU_conn cons doms 1 in

  run store cons doms
    [
      (* If a node doesn't exist, everyone gets ENOENT: *)
      (dom0, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ; (domU, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ; (* If dom0 makes a node, suddenly domU gets EACCES: *)
      (dom0, none, (Write, ["/a/b"; "hello"]), (Write, ["OK"]))
    ; (domU, none, (Read, ["/a/b"]), (Error, ["EACCES"]))
    ; (* dom0 can also see the implicit path created: *)
      (dom0, none, (Read, ["/a"]), (Read, [""]))
    ; (* domU gets EACCES: *)
      (domU, none, (Read, ["/a"]), (Error, ["EACCES"]))
    ]

(* We do not preserve ordering of nodes in a directory *)
let _test_directory_order () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  (* Create nodes in a particular order and check 'directory' preserves the ordering *)
  run store cons doms
    [
      (dom0, none, (Write, ["/a/2/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/a/1"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Write, ["/a/3"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Directory, ["/a"]), (Directory, [""]))
    ]

let example_acl = ["r5"; "w2"; "b3"]

(* Check that getperms(setperms(x)) = x *)
let test_setperms_getperms () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (dom0, none, (Getperms, ["/foo"]), (Getperms, ["r5"; "w2"; "b3"]))
    ]

(* Check that no domain other than dom0 can change owners of the node
   even if another domain has read/write access *)
let test_setperms_owner () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom2 = create_domU_conn cons doms 2 in
  let dom5 = create_domU_conn cons doms 5 in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (* owned by dom5, so dom2 can't setperms *)
      (dom2, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Error, ["EACCES"]))
    ; (* dom5 can't change owners since it's not dom0 - XSA-115 *)
      (dom5, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Error, ["EACCES"]))
    ; (* only dom0 can change owners - XSA-115 *)
      (dom0, none, (Setperms, ["/foo"; "r2"; "w2"; "b3"]), (Setperms, ["OK"]))
    ]

(* Check that mkdir creates usable nodes *)
let test_mkdir () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Read, ["/a/b"]), (Error, ["ENOENT"]))
    ; (dom0, none, (Read, ["/a"]), (Error, ["ENOENT"]))
    ] ;
  let tid = start_transaction store cons doms dom0 in
  run store cons doms
    [
      (dom0, tid, (Mkdir, ["/bench/local/domain/0"]), (Mkdir, ["OK"]))
    ; ( dom0
      , tid
      , (Setperms, ["/bench/local/domain/0"; "r5"; "w2"; "b3"])
      , (Setperms, ["OK"])
      )
    ; (dom0, tid, (Read, ["/bench/local/domain/0"]), (Read, [""]))
    ; (dom0, tid, (Transaction_end, ["T"]), (Transaction_end, ["OK"]))
    ]

(* Check that I can read an empty value *)
let test_empty () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Write, ["/a"; ""]), (Write, ["OK"]))
    ; (dom0, none, (Read, ["/a"]), (Read, ["\000"]))
    ]

(* rm of a missing node from an existing parent should succeed
   rm of a missing node from a missing parent should ENOENT *)
let test_rm () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  run store cons doms
    [
      (dom0, none, (Rm, ["/a"]), (Rm, ["OK"]))
    ; (dom0, none, (Rm, ["/a/b"]), (Error, ["ENOENT"]))
    ; (dom0, none, (Write, ["/a"; "hello"]), (Write, ["OK"]))
    ; (dom0, none, (Rm, ["/a/b"]), (Rm, ["OK"]))
    ]

(* Check that dom0 can grant dom1 access to dom2's nodes,
   without which it wouldn't have access. *)
let test_set_target () =
  let store, doms, cons = initialize () in
  let dom0 = create_dom0_conn cons doms in
  let dom7 = create_domU_conn cons doms 7 in
  run store cons doms
    [
      (dom0, none, (Write, ["/foo"; "bar"]), (Write, ["OK"]))
    ; (dom0, none, (Setperms, ["/foo"] @ example_acl), (Setperms, ["OK"]))
    ; (dom7, none, (Write, ["/foo"; "bar"]), (Error, ["EACCES"]))
    ; (dom0, none, (Set_target, ["7"; "5"]), (Set_target, ["OK"]))
    ; (* Any other domain can't use Set_target *)
      (dom7, none, (Set_target, ["7"; "5"]), (Error, ["EACCES"]))
    ; (dom7, none, (Write, ["/foo"; "bar"]), (Write, ["OK"]))
    ]

let () =
  Alcotest.run "Test RRD library"
    [
      ( "Basic tests"
      , [
          ("test_implicit_create", `Quick, test_implicit_create)
        ; ("getperms(setperms)", `Quick, test_setperms_getperms)
        ; ("test_setperms_owner", `Quick, test_setperms_owner)
        ; ("test_mkdir", `Quick, test_mkdir)
        ; ("test_empty", `Quick, test_empty)
        ; ("test_rm", `Quick, test_rm)
        ; ("test_set_target", `Quick, test_set_target)
        ]
      )
    ]
