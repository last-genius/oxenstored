(*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

module Op = struct include Op end

module Packet = struct include Packet end

type packet_class = CommandReply | Watchevent

module BoundedQueue : sig
  type ('a, 'b) t

  type limit = {command_reply_limit: int; watch_event_limit: int}

  type class_count = {
      mutable command_reply_count: int
    ; mutable watch_event_count: int
  }

  val create : capacity:int -> classify:('a -> 'b) -> limit:limit -> ('a, 'b) t
  (** [create ~capacity ~classify ~limit] creates a queue with maximum [capacity] elements.
      	    This is burst capacity, each element is further classified according to [classify],
      	    and each class can have its own [limit].
      	    [capacity] is enforced as an overall limit.
      	    The [limit] can be dynamic, and can be smaller than the number of elements already queued of that class,
      	    in which case those elements are considered to use "burst capacity".
      	  *)

  val clear : ('a, 'b) t -> unit
  (** [clear q] discards all elements from [q] *)

  val can_push : ('a, packet_class) t -> packet_class -> bool
  (** [can_push q] when [length q < capacity].	*)

  val push : 'a -> ('a, packet_class) t -> unit option
  (** [push e q] adds [e] at the end of queue [q] if [can_push q], or returns [None]. *)

  val pop : ('a, packet_class) t -> 'a
  (** [pop q] removes and returns first element in [q], or raises [Queue.Empty]. *)

  val peek : ('a, packet_class) t -> 'a
  (** [peek q] returns the first element in [q], or raises [Queue.Empty].  *)

  val length : ('a, packet_class) t -> int
  (** [length q] returns the current number of elements in [q] *)

  val debug : (packet_class -> string) -> (_, packet_class) t -> string
  (** [debug string_of_class q] prints queue usage statistics in an unspecified internal format. *)
end = struct
  type limit = {command_reply_limit: int; watch_event_limit: int}

  type class_count = {
      mutable command_reply_count: int
    ; mutable watch_event_count: int
  }

  type ('a, 'b) t = {
      q: 'a Queue.t
    ; capacity: int
    ; classify: 'a -> 'b
    ; limit: limit
    ; class_count: class_count
  }

  let create ~capacity ~classify ~limit =
    {
      capacity
    ; q= Queue.create ()
    ; classify
    ; limit
    ; class_count= {command_reply_count= 0; watch_event_count= 0}
    }

  let set_count t classification v =
    match classification with
    | CommandReply ->
        t.class_count.command_reply_count <- v
    | Watchevent ->
        t.class_count.watch_event_count <- v

  let get_count t classification =
    match classification with
    | CommandReply ->
        t.class_count.command_reply_count
    | Watchevent ->
        t.class_count.watch_event_count

  let can_push_internal t classification class_count =
    let limit =
      match classification with
      | CommandReply ->
          t.limit.command_reply_limit
      | Watchevent ->
          t.limit.watch_event_limit
    in
    Queue.length t.q < t.capacity && class_count < limit

  let ok = Some ()

  let push e t =
    let classification = t.classify e in
    let class_count = get_count t classification in
    if can_push_internal t classification class_count then (
      Queue.push e t.q ;
      set_count t classification (class_count + 1) ;
      ok
    ) else
      None

  let can_push t classification =
    can_push_internal t classification @@ get_count t classification

  let clear t =
    Queue.clear t.q ; set_count t Watchevent 0 ; set_count t CommandReply 0

  let pop t =
    let e = Queue.pop t.q in
    let classification = t.classify e in
    set_count t classification (get_count t classification - 1) ;
    e

  let peek t = Queue.peek t.q

  let length t = Queue.length t.q

  let debug string_of_class t =
    let b = Buffer.create 128 in
    Printf.bprintf b "BoundedQueue capacity: %d, used: {" t.capacity ;
    Printf.bprintf b "\t%s: %d"
      (string_of_class Watchevent)
      t.class_count.watch_event_count ;
    Printf.bprintf b "\t%s: %d"
      (string_of_class CommandReply)
      t.class_count.command_reply_count ;
    Printf.bprintf b "}" ;
    Buffer.contents b
end

exception End_of_file

exception Eagain

exception Noent

exception Invalid

exception Reconnect

let _ = Callback.register_exception "Xb.Reconnect" Reconnect

type backend_mmap = {
    mmap: Xenmmap.t (* mmaped interface = xs_ring *)
  ; eventchn_notify: unit -> unit (* function to notify through eventchn *)
  ; mutable work_again: bool
}

type backend_fd = {fd: Unix.file_descr}

type backend = Fd of backend_fd | Xenmmap of backend_mmap

type partial_buf = HaveHdr of Partial.pkt | NoHdr of int * bytes

(*
	separate capacity reservation for replies and watch events:
	this allows a domain to keep working even when under a constant flood of
	watch events
*)
type capacity = {maxoutstanding: int; maxwatchevents: int}

module Queue = BoundedQueue

let string_of_packet_class = function
  | CommandReply ->
      "command_reply"
  | Watchevent ->
      "watch_event"

type t = {
    backend: backend
  ; pkt_out: (Packet.t, packet_class) Queue.t
  ; mutable partial_in: partial_buf
  ; mutable partial_out: string
  ; capacity: capacity
}

let to_read con =
  match con.partial_in with
  | HaveHdr partial_pkt ->
      Partial.to_complete partial_pkt
  | NoHdr (i, _) ->
      i

let debug t =
  Printf.sprintf
    "XenBus state: partial_in: %d needed, partial_out: %d bytes, pkt_out: %d \
     packets, %s"
    (to_read t)
    (String.length t.partial_out)
    (Queue.length t.pkt_out)
    (BoundedQueue.debug string_of_packet_class t.pkt_out)

let init_partial_in () =
  NoHdr (Partial.header_size (), Bytes.make (Partial.header_size ()) '\000')

let reconnect t =
  match t.backend with
  | Fd _ ->
      (* should never happen, so close the connection *)
      raise End_of_file
  | Xenmmap backend ->
      Xs_ring.close Xenmmap.(to_interface backend.mmap) ;
      backend.eventchn_notify () ;
      (* Clear our old connection state *)
      Queue.clear t.pkt_out ;
      t.partial_in <- init_partial_in () ;
      t.partial_out <- ""

let queue con pkt = Queue.push pkt con.pkt_out

let read_fd back _con b len =
  let rd = Unix.read back.fd b 0 len in
  if rd = 0 then
    raise End_of_file ;
  rd

let read_mmap back _con b len =
  let s = Bytes.make len '\000' in
  let rd = Xs_ring.read Xenmmap.(to_interface back.mmap) s len in
  Bytes.blit s 0 b 0 rd ;
  back.work_again <- rd > 0 ;
  if rd > 0 then
    back.eventchn_notify () ;
  rd

let read con b len =
  match con.backend with
  | Fd backfd ->
      read_fd backfd con b len
  | Xenmmap backmmap ->
      read_mmap backmmap con b len

let write_fd back _con b len = Unix.write_substring back.fd b 0 len

let write_mmap back _con s len =
  let ws = Xs_ring.write_substring Xenmmap.(to_interface back.mmap) s len in
  if ws > 0 then
    back.eventchn_notify () ;
  ws

let write con s len =
  match con.backend with
  | Fd backfd ->
      write_fd backfd con s len
  | Xenmmap backmmap ->
      write_mmap backmmap con s len

(* NB: can throw Reconnect *)
let output con =
  (* get the output string from a string_of(packet) or partial_out *)
  let s =
    if String.length con.partial_out > 0 then
      con.partial_out
    else if Queue.length con.pkt_out > 0 then
      let pkt = Queue.pop con.pkt_out in
      Packet.to_string pkt
    else
      ""
  in
  (* send data from s, and save the unsent data to partial_out *)
  ( if s <> "" then
      let len = String.length s in
      let sz = write con s len in
      let left = String.sub s sz (len - sz) in
      con.partial_out <- left
  ) ;
  (* after sending one packet, partial is empty *)
  con.partial_out = ""

(* we can only process an input packet if we're guaranteed to have room
   to store the response packet *)
let can_input con = Queue.can_push con.pkt_out CommandReply

(* NB: can throw Reconnect *)
let input con =
  if not (can_input con) then
    None
  else
    let to_read = to_read con in

    (* try to get more data from input stream *)
    let b = Bytes.make to_read '\000' in
    let sz = if to_read > 0 then read con b to_read else 0 in

    match con.partial_in with
    | HaveHdr partial_pkt ->
        (* we complete the data *)
        if sz > 0 then
          Partial.append partial_pkt (Bytes.to_string b) sz ;
        if Partial.to_complete partial_pkt = 0 then (
          let pkt = Packet.of_partialpkt partial_pkt in
          con.partial_in <- init_partial_in () ;
          Some pkt
        ) else
          None
    | NoHdr (i, buf) ->
        (* we complete the partial header *)
        if sz > 0 then
          Bytes.blit b 0 buf (Partial.header_size () - i) sz ;
        con.partial_in <-
          ( if sz = i then
              HaveHdr (Partial.of_string (Bytes.to_string buf))
            else
              NoHdr (i - sz, buf)
          ) ;
        None

let classify t =
  match t.Packet.ty with Op.Watchevent -> Watchevent | _ -> CommandReply

let newcon ~capacity backend =
  let limit =
    Queue.
      {
        command_reply_limit= capacity.maxoutstanding
      ; watch_event_limit= capacity.maxwatchevents
      }
  in
  {
    backend
  ; pkt_out=
      Queue.create
        ~capacity:(capacity.maxoutstanding + capacity.maxwatchevents)
        ~classify ~limit
  ; partial_in= init_partial_in ()
  ; partial_out= ""
  ; capacity
  }

let open_fd fd = newcon (Fd {fd})

let open_mmap mmap notifyfct =
  (* Advertise XENSTORE_SERVER_FEATURE_RECONNECTION *)
  Xs_ring.set_server_features
    (Xenmmap.to_interface mmap)
    (Xs_ring.Server_features.singleton Xs_ring.Server_feature.Reconnection) ;
  newcon (Xenmmap {mmap; eventchn_notify= notifyfct; work_again= false})

let close con =
  match con.backend with
  | Fd backend ->
      Unix.close backend.fd
  | Xenmmap backend ->
      Xenmmap.unmap backend.mmap

let is_fd con = match con.backend with Fd _ -> true | Xenmmap _ -> false

let is_mmap con = not (is_fd con)

let output_len con = Queue.length con.pkt_out

let has_new_output con = Queue.length con.pkt_out > 0

let has_old_output con = String.length con.partial_out > 0

let has_output con = has_new_output con || has_old_output con

let peek_output con = Queue.peek con.pkt_out

let has_partial_input con =
  match con.partial_in with
  | HaveHdr _ ->
      true
  | NoHdr (n, _) ->
      n < Partial.header_size ()

let has_more_input con =
  match con.backend with Fd _ -> false | Xenmmap backend -> backend.work_again

let is_selectable con =
  match con.backend with Fd _ -> true | Xenmmap _ -> false

let get_fd con =
  match con.backend with
  | Fd backend ->
      backend.fd
  | Xenmmap _ ->
      raise (Failure "get_fd")
