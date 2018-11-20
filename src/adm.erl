-module(adm).
-description('ADM Monitoring Bootcode').
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1,main/1]).

main(X)   -> mad:repl(X).
tables() -> [ fs ].
opt()    -> [ set, named_table, { keypos, 1 }, public ].
static() ->   { dir, "priv/static", [] }.
n2o()    ->   { dir, "deps/n2o/priv", [] }.
port()   -> [ { port, application:get_env(n2o,port,8108)  } ].
points() -> cowboy_router:compile([{'_',
            [ {"/static/[...]",       cowboy_static,  static()},
              {"/n2o/[...]",          cowboy_static,  n2o()},
              {"/ws/[...]",           n2o_cowboy2,    []} ]} ]).

stop(_)    -> ok.
start(_,_) -> cowboy:start_clear(http, port(), #{ env => #{dispatch => points()} }),
              supervisor:start_link({local,adm},adm,[]).
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              kvs:join(), {ok, {{one_for_one, 5, 10}, [] }}.
