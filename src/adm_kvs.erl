-module(adm_kvs).
-compile(export_all).
-include_lib("kvs/include/entry.hrl").
-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include_lib("kvs/include/feed.hrl").
-include_lib("kvs/include/kvs.hrl").

event(init) -> [ self() ! {direct,{atom,X}} || X <- [streams,datawin,binders,boot] ],
               wf:update(disc,hd(string:tokens(os:cmd("du -hs Mnesia."++lists:concat([node()])),"\t"))),
               wf:update(ram, case os:type() of
                                             {_,darwin} -> [L,C,R]=string:tokens(lists:filter(fun(X) ->
                                              lists:member(X,"0123456789M") end,os:cmd("top -l 1 -s 0 | grep PhysMem")),"M"),
                                              lists:concat([L,"/",list_to_integer(L)+list_to_integer(R),"M"]);
                                              {_,linux} -> [T,U,_,_,B,C] = lists:sublist(string:tokens(os:cmd("free")," \n"),8,6),
                                                          lists:concat([(wf:to_integer(U)-(wf:to_integer(B)+wf:to_integer(C))) div 1000,"/",wf:to_integer(T) div 1000,"M"]);
                                                    _ -> "N/A" end),
              wf:update(date,bpe_date:date_to_string(bpe_date:today())),
              wf:update(enode,lists:concat([node()])),
              wf:update(session,n2o_session:session_id());
event({binder,Name}) -> wf:update(datawin,fold_(table_fold(Name,first(Name),20,[])));
event({stream,Name}) -> Feed = case element(3,kvs:table(Name)) of
                            true -> feed;
                            F -> F end,
                        case kvs:get(Feed,Name) of
                             {ok,V} -> wf:update(datawin,fold(Name,element(#container.top,V),20));
                             _ -> wf:update(datawin,fold(Name,0,0)) end;
event({atom,X}) -> wf:update(X,?MODULE:X());
event(U) -> io:format("Unknown Event: ~p~n\n",[U]).

pro() ->    [ #script { src = "/static/adm.min.js"} ].
dev()  -> [ [ #script { src = lists:concat(["/n2o/protocols/",X,".js"])} || X <- [bert,nitrogen] ],
            [ #script { src = lists:concat(["/n2o/",Y,".js"])}           || Y <- [bullet,n2o,utf8,validation] ] ].
main() ->     #dtl    { file = "index", app=adm,
                        bindings = []}.

tables() -> [ element(2,T) || T <- kvs:tables() ].
containers() -> [ element(2,T) || T <- kvs:tables(), record_info(fields,container) -- element(4,T) == [] ].
iterators() -> [ element(2,T) || T <- kvs:tables(), record_info(fields,iterator) -- element(4,T) == [] ].
noniterators() -> [ element(2,T) || T <- kvs:tables(), record_info(fields,iterator) -- element(4,T) /= [] ].
binders_() -> [ X || {X,_}<- kvs:containers() ].
bsize(Config) -> lists:sum([ mnesia:table_info(Name,size) || #block{name=Name} <- Config ]).
blocks(Config) -> length(Config)+1.
row(Name) -> Config = kvs:config(Name), StrName = lists:concat([Name]),
             #tr{id=Name,cells=[#td{body=#link{href="#",onclick="setup_window("++StrName++");",body=StrName,postback={stream,Name}}},
                                #td{body=lists:concat([blocks(Config)])},
                                #td{body=lists:concat([bsize(Config)+mnesia:table_info(Name,size)])}]}.

row2(Name) -> Config = kvs:config(Name), StrName = lists:concat([Name]),
             #tr{id=Name,cells=[#td{body=#link{href="#",body=StrName,onclick="setup_window("++StrName++");",postback={binder,Name}}},
                                #td{body=lists:concat([kvs:count(Name)])}]}.

row3(Record) ->
             Name = element(1,Record),
             Id = element(2,Record),
             Table = kvs:table(Name),
             #tr{id=Id,cells=[#td{body=#b{body=io_lib:format("~tp",[Id])}},
                              #td{body=wf:jse(lists:concat([io_lib:format("~tp",[Record])]))}]}.

boot_() ->
  #panel{class=wizard,id=boot, body=[#h2{body="BOOT"},
      #panel{body="["++string:join([ atom_to_list(T)||T<-tables()],", ")++"]"}]}.

boot() ->
  #panel{class=wizard,id=boot, body=[#h2{body="CORE"},
      #table{body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Size"}]}},
                   #tbody{body=[row2(Name)||Name<- tables()-- (iterators()++containers()) ]}]}]}.

streams() ->
  #panel{class=wizard,id=streams, body=[#h2{body="STREAMS"},
      #table{body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Blocks"},#th{body="Size"}]}},
                   #tbody{body=[row(Name)||Name<- iterators() ]}]}]}.

first(Name) -> {atomic,Key} = mnesia:transaction(fun() -> mnesia:first(Name) end), Key.
table_fold(_,_,0,Acc) -> Acc;
table_fold(Name,'$end_of_table',Count,Acc) -> Acc;
table_fold(Name,First,Count,Acc) ->
   Data = case kvs:get(Name,First) of
        {ok,D} -> D;
        _ -> [] end,
   {atomic,Key} = mnesia:transaction(fun() -> mnesia:next(Name,First) end),
   case Key of '$end_of_table' -> [Data|Acc];
                           Key -> table_fold(Name,Key,Count-1,[Data|Acc]) end.

datawin() -> fold(group,20,10).
fold_(Traverse) ->
  #panel{class=wizard,id=datawin, body=[#h2{body="DATA WINDOW"},
      #table{style="width:100%;",
             body=[#thead{body=#tr{cells=[#th{body="No"},#th{body="Record"}]}},
                   #tbody{body=[row3(Record)||Record<- Traverse]},
                   #tfoot{body=[#th{body=#link{body="prev"}},#th{body=#link{body="next",postback={shift}}}]}
                   ]}]}.

fold(Table,Start,Count) ->
  Traverse = kvs:traversal(Table,Start,Count,#iterator.prev,#kvs{mod=store_mnesia}),
  fold_(Traverse).

binders() ->
  #panel{class=wizard,id=binders, body=[#h2{body="BINDERS"},
      #table{body=[#thead{body=#tr{cells=[#th{body="Name"},#th{body="Size"}]}},
                   #tbody{body=[row2(Name)||Name<- containers()]}]}]}.
