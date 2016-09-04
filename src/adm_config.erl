-module(adm_config).
-compile(export_all).

log_level() -> info.
log_modules() -> % N2O stack + ADM
  [
    n2o_proto,
    n2o_stream,
    n2o_session,
    n2o_nitrogen,
    adm,
    adm_kvs
  ].
