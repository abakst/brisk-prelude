%% proctype A:
%%   ds_ddg0 := recv[Ping]
%%   match ds_ddg0:
%%     case (Ping(q)):
%%       send[ProcessId](q,A)
%%       msg := recv[ABigRecord]
%%       anf0 := case msg of
%%                 Foo ds_ddga ds_ddgb ->
%%                   ds_ddgb
%%       anf1 := Unit
%%       send[Tuple](anf0,anf1)
%%     __DEFAULT__:
%%       abort
%% proctype B:
%%   anf0 := (Ping(B))
%%   send[Ping](A,anf0)
%%   msg := recv[ABigRecord]
%%   match msg:
%%     case (Foo(ds_ddg4,ds_ddg5)):
%%       anf1 := (Foo(0,B))
%%       send[ABigRecord](ds_ddg5,anf1)
%%   q := recv[Tuple]

rewrite_query(T,skip,Ind,Name) :-
        Ind=[],
        %%%%% proctype A:
        TA=seq([ recv(A, type(tyCon(ty__Ping)), ds_ddg0),
                 cases(A, ds_ddg0,
                       [ case(A, cstr__Ping(q),
                              seq([ send(A, tyCon(ty__ProcessId), e_var(q), A),
                                    recv(A, type(tyCon(ty__ABigRecord)), msg),
                                    assign(A, anf0, cases(A, msg,
                                                          [ case(A, cstr__Foo(ds_ddga, ds_ddgb),
                                                                 ds_ddgb)])),
                                    assign(A, anf1, cstr__Unit),
                                    send(A, tyCon(ty__Tuple), e_var(anf0), anf1)]))],
                       default(A, die(A)))]),
        %%%%% proctype B:
        TB=seq([ assign(B, anf0, cstr__Ping(B)),
                 send(B, tyCon(ty__Ping), e_pid(A), anf0),
                 recv(B, type(tyCon(ty__ABigRecord)), msg),
                 cases(B, msg,
                       [ case(B, cstr__Foo(ds_ddg4, ds_ddg5),
                              seq([ assign(B, anf1, cstr__Foo(0, B)),
                                    send(B, tyCon(ty__ABigRecord), e_var(ds_ddg5), anf1)]))]),
                 recv(B, type(tyCon(ty__Tuple)), q)]),
        %%%%%
        T=par([TA,TB]),
        Name='simple-cases'.