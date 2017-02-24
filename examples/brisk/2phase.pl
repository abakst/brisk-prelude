%% proctype A:
%%   ds_dbw6 := Unit
%%   for x in %B_Set:
%%     match ds_dbw6:
%%       case Unit:
%%         anf0 := (Tuple(%A,fn))
%%         send[Tuple[ProcessId String]](x,anf0)
%%   x := 0
%%   for y in %B_Set:
%%     msg := recv[AcceptorResponse]
%%     match msg:
%%       case (Accept(ds_dbwf)):
%%         x := {@Int}
%%       case Reject:
%%         skip
%%   match {@Bool}:
%%     case False:
%%       ds_dbw4 := Unit
%%       for x in %B_Set:
%%         match ds_dbw4:
%%           case Unit:
%%             anf1 := (Rollback(%A))
%%             send[CoordMessage](x,anf1)
%%     case True:
%%       ds_dbw2 := Unit
%%       for x in %B_Set:
%%         match ds_dbw2:
%%           case Unit:
%%             anf2 := (Commit(%A))
%%             send[CoordMessage](x,anf2)
%% proctype (B:B_Set):
%%   ds_dbwi := recv[Tuple[ProcessId String]]
%%   match ds_dbwi:
%%     case (Tuple(who,fn)):
%%       anf0 := case {@Process Bool} of
%%                 False ->
%%                   Reject
%%                 True ->
%%                   (Accept(B))
%%       send[AcceptorResponse](who,anf0)
%%       msg := recv[CoordMessage]
%%       anf1 := case msg of
%%                 Commit p ->
%%                   p
%%                 Rollback p ->
%%                   p
%%       send[AcceptorAck](anf1,ACK)
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