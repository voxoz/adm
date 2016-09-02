-module(adm_kvs).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/kvs.hrl").

event(init) -> [ wf:update(X,?MODULE:X()) || X <- [streams,datawin,binders,boot] ];
event({stream,Name}) -> case kvs:get(feed,Name) of 
                             {ok,V} -> wf:update(datawin,fold(Name,V#feed.top,20));
                             _ -> wf:update(datawin,fold(Name,0,0)) end;
event(U) -> io:format("Unknown Event: ~p~n",[U]).

pro() ->    [ #script { src = "/static/adm.min.js"} ].
dev()  -> [ [ #script { src = lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script { src = lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,n2o,utf8,validation] ] ].
main() ->     #dtl    { file = "index", app=adm,
                        bindings = [{body,[]},
                                    {javascript,dev()}]}.

tables() -> [ element(2,T) || T <- kvs:tables() ].
binders_() -> [ X || {X,_}<- kvs:containers() ].
bsize(Config) -> lists:sum([ mnesia:table_info(Name,size) || #block{name=Name} <- Config ]).
blocks(Config) -> length(Config)+1.
row(Name) -> Config = kvs:config(Name), StrName = lists:concat([Name]),
             #tr{id=Name,cells=[#td{body=#link{id=Name,href="#",onclick="setup_window("++StrName++");",body=StrName,postback={stream,Name}}},
                                #td{body=lists:concat([blocks(Config)])},
                                #td{body=lists:concat([bsize(Config)+mnesia:table_info(Name,size)])}]}.

row2(Name) -> Config = kvs:config(Name), StrName = lists:concat([Name]),
             #tr{id=Name,cells=[#td{body=#link{id=Name,href="#",body=StrName,onclick="setup_window("++StrName++");",postback={binder,Name}}},
                                #td{body=lists:concat([kvs:count(Name)])}]}.

row3(Record) -> io:format("R: ~s~n",[io_lib:format("~p",[Record])]),
             Name = element(1,Record),
             Id = element(2,Record),
             Table = kvs:table(Name),
             #tr{id=Id,cells=[#td{body=#b{body=lists:concat([Id])}},
                              #td{body=wf:jse(lists:concat([io_lib:format("~tp",[Record])]))}]}.

boot() ->
  #panel{class=wizard,id=boot, body=[#h2{body="BOOT"},
      #panel{style="width:550px;font-size:12pt;background-color:white;",body="["++string:join([ atom_to_list(T)||T<-tables()],", ")++"]"}]}.

streams() ->
  #panel{class=wizard,id=streams, body=[#h2{body="STREAMS"},
      #table{style="border-style:solid;border-width:1px;padding:20px;",
             body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Blocks"},#th{body="Size"}]}},
                   #tbody{body=[row(Name)||Name<- tables()-- binders_()]}]}]}.

datawin() -> fold(process,100,10).
fold(Table,Start,Count) ->
  Traverse = kvs:traversal(Table,Start,Count,#iterator.prev,#kvs{mod=store_mnesia}),
  #panel{class=wizard,id=datawin, body=[#h2{body="DATA WINDOW"},
      #table{style="width:100%;border-style:solid;border-width:1px;padding:20px;",
             body=[#thead{body=#tr{cells=[#th{body="No"},#th{body="Record"}]}},
                   #tbody{body=[row3(Record)||Record<- Traverse]},
                   #tfoot{body=[#th{body="prev"},#th{body="next"}]}
                   ]}]}.

binders() ->
  #panel{class=wizard,id=binders, body=[#h2{body="BINDERS"},
      #table{style="border-style:solid;border-width:1px;padding:20px;",
             body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Size"}]}},
                   #tbody{body=[row2(Name)||Name<- binders_()]}]}]}.
