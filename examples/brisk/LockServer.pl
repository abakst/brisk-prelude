%% proctype A:
%%   ds_ddbB := Unit
%%   for ds_ddbC in %B_Set:
%%     ds_ddbG := recv[Lock]
%%     match ds_ddbG:
%%       case (Lock(p)):
%%         send[Grant](p,Granted)
%%         msg := recvFrom[SelfSigned[Release]](p)
%%         match msg:
%%           case (SelfSigned(_,pay)):
%%             ds_ddbD := pay
%%         match ds_ddbD:
%%           case Release:
%%             ds_ddbB := Unit
%% proctype (B:B_Set):
%%   anf0 := (Lock(B))
%%   send[Lock](%A,anf0)
%%   ds_ddbR := recv[Grant]
%%   match ds_ddbR:
%%     case Granted:
%%       anf1 := (SelfSigned(B,Release))
%%       send[SelfSigned[Release]](%A,anf1)
rewrite_query(T,skip,Ind,Name) :-
        Ind=[],
        par([TA,TB]),
        TA=seq([ assign(A, ds_ddbB, cstr__Unit),
                 for(A, ds_ddbC, set(B_Set), seq([ recv(A, type(tyCon(ty__Lock)), ds_ddbG),
                                                   cases(A, ds_ddbG, [ case(A, cstr__Lock(P), seq([ send(A, e_pid(P), tyCon(ty__Grant), cstr__Granted),
                                                                                                    recv(A, P, type(tyCon(ty__SelfSigned, tyCon(ty__Release))), msg),
                                                                                                    cases(A, msg, [ case(A, cstr__SelfSigned(_, Pay), assign(A, ds_ddbD, Pay))]),
                                                                                                    cases(A, ds_ddbD, [ case(A, cstr__Release, assign(A, ds_ddbB, cstr__Unit))])]))])]))]),
            
        TB=sym(B, set(B_Set), seq([ assign(B, anf0, cstr__Lock(B)),
                                       send(B, A, tyCon(ty__Lock), anf0),
                                       recv(B, type(tyCon(ty__Grant)), ds_ddbR),
                                       cases(B, ds_ddbR, [ case(B, cstr__Granted, seq([ assign(B, anf1, cstr__SelfSigned(B, cstr__Release)),
                                                                                        send(B, A, tyCon(ty__SelfSigned, tyCon(ty__Release)), anf1)]))])])),
        A=a,
        B_Set=bs,
        Name='Lock-Server'.
        