/* SPDX-License-Identifier: LGPL-2.1-only WITH OCaml-LGPL-linking-exception */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/custom.h>

#include <xenmanage.h>

static inline xenmanage_handle *
xenmanage_handle_of_val (value v)
{
        xenmanage_handle *handle = *(xenmanage_handle **) Data_custom_val (v);

        return handle;
}

static void
xenmanage_finalize (value v)
{
        xenmanage_handle *handle = xenmanage_handle_of_val (v);

        xenmanage_close (handle);
}

static struct custom_operations xenmanage_ops = {
        .identifier = "xenmanage.ops",
        .finalize = xenmanage_finalize,
        .compare = custom_compare_default,      /* Can't compare     */
        .hash = custom_hash_default,    /* Can't hash        */
        .serialize = custom_serialize_default,  /* Can't serialize   */
        .deserialize = custom_deserialize_default,      /* Can't deserialize */
        .compare_ext = custom_compare_ext_default,      /* Can't compare     */
};

static void Noreturn
xenmanage_failwith (xenmanage_handle * handle, const char *func, unsigned int line)
{
        CAMLparam0 ();
        CAMLlocal1 (msg);
        char *str = NULL;

        int ret = asprintf (&str,
                            "Encountered an error (errno %d): %s - called from %s:%u",
                            errno, strerror (errno), func, line);

        if (!*str || (ret == -1))
                caml_raise_out_of_memory ();

        msg = caml_copy_string (str);
        free (str);

        caml_raise_with_arg (*caml_named_value ("xenmanage.error"), msg);
}

#define xenmanage_failwith(handle) xenmanage_failwith(handle, __func__, __LINE__)

CAMLprim value
stub_xenmanage_open (value unit)
{
        CAMLparam1 (unit);
        CAMLlocal1 (result);
        xenmanage_handle *handle;

        result = caml_alloc_custom (&xenmanage_ops, sizeof (handle), 0, 1);

        caml_enter_blocking_section ();
        handle = xenmanage_open (NULL, NULL, 0);
        caml_leave_blocking_section ();

        if (!handle)
                xenmanage_failwith (handle);

        *(xenmanage_handle **) Data_custom_val (result) = handle;

        CAMLreturn (result);
}

static value
alloc_xenmanage_info (unsigned int domid, unsigned int state, unsigned int unique_id)
{
        CAMLparam0 ();
        CAMLlocal1 (result);

        result = caml_alloc_tuple (6);

        Store_field (result, 0, Val_int (domid));
        Store_field (result, 1, Val_int64 (unique_id));
        Store_field (result, 2, Val_bool (info->flags & XENMANAGE_GETDOMSTATE_STATE_EXIST));
        Store_field (result, 3, Val_bool (info->flags & XENMANAGE_GETDOMSTATE_STATE_SHUTDOWN));
        Store_field (result, 4, Val_bool (info->flags & XENMANAGE_GETDOMSTATE_STATE_DYING));
        Store_field (result, 5, Val_bool (info->flags & XENMANAGE_GETDOMSTATE_STATE_DEAD));

        CAMLreturn (result);
}

CAMLprim value
stub_xenmanage_get_domaininfo (value handle_val, value domid)
{
        CAMLparam2 (handle_val, domid);
        CAMLlocal1 (result);
        xenmanage_handle *handle = xenmanage_handle_of_val (handle_val);
	unsigned int state;
	uint64_t unique_id;

        int ret;
        int domid_c = Int_val (domid);

        caml_enter_blocking_section ();
        ret = xenmanage_get_domain_info (handle, domid_c, &state, &unique_id);
        caml_leave_blocking_section ();

        if (ret < 0)
                xenmanage_failwith (handle);

        result = alloc_xenmanage_info (domid_c, state, unique_id);

        CAMLreturn (result);
}

CAMLprim value
stub_xenmanage_poll_changed_domain (value handle_val)
{
        CAMLparam1 (handle_val);
        CAMLlocal1 (result);
        xenmanage_handle *handle = xenmanage_handle_of_val (handle_val);
	unsigned int state, domid_c;
	uint64_t unique_id;
        int ret;

        caml_enter_blocking_section ();
        ret = xenmanage_poll_changed_domain (handle, &domid_c, &state, &unique_id);
        caml_leave_blocking_section ();

        if (ret < 0)
                xenmanage_failwith (handle);

        result = alloc_xenmanage_info (domid_c, state, unique_id);

        CAMLreturn (result);
}
