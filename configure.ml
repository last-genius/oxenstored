let xen_run_dir=ref "/var/run/xen/"
let xen_log_dir=ref "/var/log/xen/"
let xenstored_kva= ref "/proc/xen/xsd_kva"
let xenstored_port=ref "/proc/xen/xsd_port"

let args =
  [
    ("XEN_RUN_DIR", Arg.Set_string xen_run_dir, "")
  ; ("XEN_LOG_DIR", Arg.Set_string xen_log_dir, "")
  ; ( "XENSTORED_KVA", Arg.Set_string xenstored_kva, "")
  ; ( "XENSTORED_PORT", Arg.Set_string xenstored_port, "")
  ]
  |> Arg.align

let () = Arg.parse args ignore "";
