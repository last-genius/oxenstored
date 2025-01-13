(* SPDX-License-Identifier: LGPL-2.1-only WITH OCaml-LGPL-linking-exception *)

exception Error of string

type domid = int

type unique_domid = Int64.t

type handle

type domaininfo = {
    domid: domid
  ; unique_id: unique_domid
  ; existing: bool
  ; shutdown: bool
  ; dying: bool
  ; dead: bool
}

external interface_open : unit -> handle = "stub_xenmanage_open"

external domain_getinfo : handle -> domid -> domaininfo
  = "stub_xenmanage_get_domain_info"

external poll_changed_domain : handle -> domaininfo array
  = "xenmanage_poll_changed_domain"

let _ = Callback.register_exception "xenmanage.error" (Error "register_callback")
