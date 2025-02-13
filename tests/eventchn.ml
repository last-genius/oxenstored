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

type handle = Unix.file_descr * int ref

let devnull = Unix.openfile "/dev/null" [] 0

let init ?(cloexec = true) () =
  let _ = cloexec in
  (devnull, ref 0)

let fd (h, _) = h

type t = int

let notify _h _ = ()

let bind_interdomain (_h, port) _domid _remote_port = incr port ; !port

let bind_virq (_h, port) = incr port ; !port

let bind_dom_exc_virq handle = bind_virq handle

let unbind _ _ = ()

let pending (_h, port) = !port

let unmask _ _ = ()

let to_int x = x

let of_int x = x
