-module(index).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").

event(init) -> wf:update(streams,streams()),
               wf:update(boot,boot()),
               wf:update(binders,binders()).

prod() ->   [ #script { src = "/static/adm.min.js"} ].
dev()  -> [ [ #script { src = lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script { src = lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,n2o,ftp,utf8,validation] ] ].
main() ->     #dtl    { file = "index", app=adm,
                        bindings = [{body,[]},
                                    {javascript,(?MODULE:(wf:config(n2o,mode,dev)))()}]}.

tables() -> [ element(2,T) || T <- kvs:tables() ].
binders_() -> [ X || {X,_}<- kvs:containers() ].
bsize(Config) -> lists:sum([ mnesia:table_info(Name,size) || #block{name=Name} <- Config ]).
blocks(Config) -> length(Config)+1.
row(Name) -> Config = kvs:config(Name),
             #tr{id=Name,cells=[#td{body=#b{body=lists:concat([Name])}},
                                #td{body=lists:concat([blocks(Config)])},
                                #td{body=lists:concat([bsize(Config)+mnesia:table_info(Name,size)])}]}.

row2(Name) -> Config = kvs:config(Name),
             #tr{id=Name,cells=[#td{body=#b{body=lists:concat([Name])}},
                                #td{body=lists:concat([kvs:count(Name)])}]}.

boot() ->
  #panel{class=wizard,id=boot, body=[#h2{body="BOOT"},
      #panel{style="width:550px;font-size:12pt;background-color:white;",body="["++string:join([ atom_to_list(T)||T<-tables()],", ")++"]"}]}.

streams() ->
  #panel{class=wizard,id=streams, body=[#h2{body="STREAMS"},
      #table{style="border-style:solid;border-width:1px;padding:20px;",
             body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Blocks"},#th{body="Size"}]}},
                   #tbody{body=[row(Name)||Name<- tables()-- binders_()]}]}]}.


binders() ->
  #panel{class=wizard,id=binders, body=[#h2{body="BINDERS"},
      #table{style="border-style:solid;border-width:1px;padding:20px;",
             body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Size"}]}},
                   #tbody{body=[row2(Name)||Name<- binders_()]}]}]}.


