(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012-2014 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

type gntref = int

type domid = int

let console = 0 (* public/grant_table.h:GNTTAB_RESERVED_CONSOLE *)

let xenstore = 1 (* public/grant_table.h:GNTTAB_RESERVED_XENSTORE *)

module Gnttab = struct
  type interface = int

  let interface_open () = 1

  let interface_close _h = ()

  type grant = {domid: int; ref: gntref}

  let unmap_exn _h _mm = ()

  external fake_interface_get : unit -> Xenmmap.mmap_interface = "unsafe_stub"

  module Local_mapping = struct
    type t = Xenmmap.mmap_interface

    let to_pages interface t = Xenmmap.make t ~unmap:(unmap_exn interface)
  end

  let map_exn _interface _grant _writable = fake_interface_get ()
end

type grant = {domid: int; ref: gntref}
