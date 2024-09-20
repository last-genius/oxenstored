let xen_run_dir = ref "/var/run/xen/"

let xen_run_stored = ref "/var/run/xenstored"

let xen_log_dir = ref "/var/log/xen/"

let xen_config_dir = ref "/etc/xen"

let libexec = ref "/usr/local/lib/xen"

let args =
  [
    ("XEN_RUN_DIR", Arg.Set_string xen_run_dir, "")
  ; ("XEN_RUN_STORED", Arg.Set_string xen_run_stored, "")
  ; ("XEN_LOG_DIR", Arg.Set_string xen_log_dir, "")
  ; ("XEN_CONFIG_DIR", Arg.Set_string xen_config_dir, "")
  ; ("LIBEXEC", Arg.Set_string libexec, "")
  ]

let () =
  Arg.parse (args |> Arg.align) ignore "" ;
  List.iter2
    (fun var (name, _, _) ->
      print_endline
        (Printf.sprintf {|let %s = "%s"|} (String.lowercase_ascii name) var)
    )
    [!xen_run_dir; !xen_run_stored; !xen_log_dir; !xen_config_dir; !libexec]
    args
