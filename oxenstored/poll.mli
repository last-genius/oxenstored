(*
 * Copyright (C) 2014 Zheng Li <dev@zheng.li>
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

type event = {
    mutable read: bool
  ; mutable write: bool
  ; mutable can_read: bool
  ; mutable can_write: bool
}

val init_event : unit -> event

val poll_select :
     (Unix.file_descr * event) array
  -> float
  -> Unix.file_descr list * Unix.file_descr list
