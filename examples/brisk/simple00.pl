%% proctype A:
%%   ds_ddfZ := recv[Ping]
%%   match ds_ddfZ:
%%     case (Ping(q)):
%%       send[ProcessId](q,A)
%%       msg := recv[ABigRecord]
%%       anf0 := case msg of
%%                 Foo ds_ddg6 ds_ddg7 ->
%%                   ds_ddg7
%%       anf1 := Unit
%%       send[Tuple](anf0,anf1)
%%     __DEFAULT__:
%%       abort
%% proctype B:
%%   anf0 := (Ping(B))
%%   send[Ping](A,anf0)
%%   q := recv[ProcessId]
%%   anf1 := (Foo(0,B))
%%   send[ABigRecord](q,anf1)
%%   q := recv[Tuple]
rewrite_query(T,skip,Ind,Name) :-
        Ind=[],
        %%%%% proctype A:
        TA=seq([ recv(A, type(tyCon(ty__Ping)), ds_ddfZ),
                 cases(A, ds_ddfZ,
                       [ case(A, cstr__Ping(q),
                              seq([ send(A, tyCon(ty__ProcessId), e_var(q), A),
                                    recv(A, type(tyCon(ty__ABigRecord)), msg),
                                    assign(A, anf0, cases(A, msg, [ case(A, cstr__Foo(ds_ddg6, ds_ddg7), ds_ddg7)])),
                                    assign(A, anf1, cstr__Unit),
                                    send(A, tyCon(ty__Tuple), e_var(anf0), anf1)]))
                       ],
                       default(A, die(A)))
               ]),
        %%%%% proctype B:
        TB=seq([ assign(B, anf0, cstr__Ping(B)),
                 send(B, tyCon(ty__Ping), e_pid(A), anf0),
                 recv(B, type(tyCon(ty__ProcessId)), q),
                 assign(B, anf1, cstr__Foo(0, B)),
                 send(B, tyCon(ty__ABigRecord), e_var(q), anf1),
                 recv(B, type(tyCon(ty__Tuple)), q)
               ]),
        %%%%%
        T=par([TA,TB]),
        Name='simple-cases'.