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

(* Open the xenmanage interface. Required to be called before any other
   functions in the modules. The interface is automatically closed on GC
   cleanup of the object. *)
external interface_open : unit -> handle = "stub_xenmanage_open"

(*
 * Return state information of an existing domain.
 *
 * Returns the domain state and unique id of the given domain.
 *)
external domain_getinfo : handle -> domid -> domaininfo
  = "stub_xenmanage_get_domain_info"

(*
 * Return information of a domain having changed state recently.
 *
 * Returns the domain id, state and unique id of a domain having changed
 * state (any of the state bits was modified) since the last time information
 * for that domain was returned by this function. Only usable by callers who
 * have registered the VIRQ_DOM_EXC event (normally Xenstore).
 *)
external poll_changed_domain : handle -> domaininfo array
  = "xenmanage_poll_changed_domain"
