(* SPDX-License-Identifier: LGPL-2.1-only WITH OCaml-LGPL-linking-exception *)

module type Domain_getinfo_V1 = sig
  exception Error of string

  type domid = int

  type handle

  type domaininfo = {
      domid: domid
    ; dying: bool
    ; shutdown: bool
    ; shutdown_code: int
  }

  val interface_open : unit -> handle

  val domain_getinfo : handle -> domid -> domaininfo

  val domain_getinfolist : handle -> domaininfo array
end

module Domain_getinfo_V1_impl = struct
  exception Error of string

  type domid = int

  type handle = int

  type domaininfo = {
      domid: domid
    ; dying: bool
    ; shutdown: bool
    ; shutdown_code: int
  }

  let interface_open () = 1

  let domain_getinfo _h domid =
    {domid; dying= false; shutdown= false; shutdown_code= 1}

  let domain_getinfolist _h =
    [|{domid= 1; dying= false; shutdown= false; shutdown_code= 1}|]
end

let get_plugin_v1 () : (module Domain_getinfo_V1) =
  (module Domain_getinfo_V1_impl)
