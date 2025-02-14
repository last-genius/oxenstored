/*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
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
 */

#include <syslog.h>
#include <string.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

static int __syslog_level_table[] = {
        LOG_EMERG, LOG_ALERT, LOG_CRIT, LOG_ERR, LOG_WARNING,
        LOG_NOTICE, LOG_INFO, LOG_DEBUG
};

static int __syslog_facility_table[] = {
        LOG_AUTH, LOG_AUTHPRIV, LOG_CRON, LOG_DAEMON, LOG_FTP, LOG_KERN,
        LOG_LOCAL0, LOG_LOCAL1, LOG_LOCAL2, LOG_LOCAL3,
        LOG_LOCAL4, LOG_LOCAL5, LOG_LOCAL6, LOG_LOCAL7,
        LOG_LPR | LOG_MAIL | LOG_NEWS | LOG_SYSLOG | LOG_USER | LOG_UUCP
};

value
stub_syslog (value facility, value level, value msg)
{
        CAMLparam3 (facility, level, msg);
        char *c_msg = strdup (String_val (msg));
        char *s = c_msg, *ss;
        int c_facility = __syslog_facility_table[Int_val (facility)]
                | __syslog_level_table[Int_val (level)];

        if (!c_msg)
                caml_raise_out_of_memory ();

        /*
         * syslog() doesn't like embedded newlines, and c_msg generally
         * contains them.
         *
         * Split the message in place by converting \n to \0, and issue one
         * syslog() call per line, skipping the final iteration if c_msg ends
         * with a newline anyway.
         */
        do
          {
                  ss = strchr (s, '\n');
                  if (ss)
                          *ss = '\0';
                  else if (*s == '\0')
                          break;

                  caml_enter_blocking_section ();
                  syslog (c_facility, "%s", s);
                  caml_leave_blocking_section ();

                  s = ss + 1;
          }
        while (ss);

        free (c_msg);
        CAMLreturn (Val_unit);
}
