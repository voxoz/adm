-module(adm_routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, #cx{req=Req}=Cx) ->
    #{path:=Path}=Req,
    Fix  = route_prefix(Path),
    n2o:info(?MODULE,"Route: ~p~n",[{Fix,Path}]),
    {ok, State, Cx#cx{path=Path,module=Fix}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(_) -> adm_kvs.
