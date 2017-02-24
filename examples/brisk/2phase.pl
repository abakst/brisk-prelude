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
        TA=seq([ assign(A, ds_dbw6, cstr__Unit),
            for(A, x, set(B_Set), cases(A, ds_dbw6, [ case(A, cstr__Unit, seq([ assign(A, anf0, cstr__Tuple(A, fn)),
                                                                                send(A, e_var(x), tyCon(ty__Tuple, tyCon(ty__ProcessId), tyCon(ty__String)), anf0)]))])),
            assign(A, x, 0),
            for(A, y, set(B_Set), seq([ recv(A, type(tyCon(ty__AcceptorResponse)), msg),
                                        cases(A, msg, [ case(A, cstr__Accept(Ds_dbwf), assign(A, x, ndet)),
                                                        case(A, cstr__Reject, skip)])])),
            cases(A, ndet, [ case(A, cstr__False, seq([ assign(A, ds_dbw4, cstr__Unit),
                                                        for(A, x, set(B_Set), cases(A, ds_dbw4, [ case(A, cstr__Unit, seq([ assign(A, anf1, cstr__Rollback(A)),
                                                                                                                            send(A, e_var(x), tyCon(ty__CoordMessage), anf1)]))]))])),
                             case(A, cstr__True, seq([ assign(A, ds_dbw2, cstr__Unit),
                                                       for(A, x, set(B_Set), cases(A, ds_dbw2, [ case(A, cstr__Unit, seq([ assign(A, anf2, cstr__Commit(A)),
                                                                                                                           send(A, e_var(x), tyCon(ty__CoordMessage), anf2)]))]))]))])]),
        TB=sym(B, set(B_Set), seq([ recv(B, type(tyCon(ty__Tuple, tyCon(ty__ProcessId), tyCon(ty__String))), ds_dbwi),
                               cases(B, ds_dbwi, [ case(B, cstr__Tuple(Who, Fn), seq([ cases(B, ndet, [ case(B, cstr__False, assign(B, anf0, cstr__Reject)),
                                                                                                        case(B, cstr__True, assign(B, anf0, cstr__Accept(B)))]),
                                                                                       send(B, e_pid(Who), tyCon(ty__AcceptorResponse), anf0),
                                                                                       recv(B, type(tyCon(ty__CoordMessage)), msg),
                                                                                       cases(B, msg, [ case(B, cstr__Commit(P), assign(B, anf1, P)),
                                                                                                       case(B, cstr__Rollback(P), assign(B, anf1, P))]),
                                                                                       send(B, e_var(anf1), tyCon(ty__AcceptorAck), cstr__ACK)]))])])),
        T=par([TA,TB]),
        A=a,
        B_Set=bs,
        Ind=[],
        Name='2-phase-commit'.