%% proctype a:
%%   match {@Process Char}:
%%     case (C#(ds_d8vq)):
%%       match ds_d8vq:
%%         case #vv:
%%           anf0 := (GetBlob({@Process String}))
%%           rpc := anf0
%%         case #vv:
%%           anf1 := (AddBlob({@Process String},{@Process ByteString}))
%%           rpc := anf1
%%         __DEFAULT__:
%%           rpc := {@Process[DataNodeAPI]}
%%   anf2 := {_ ∈ %b_Set}
%%   send[DataNodeAPI](anf2,rpc)
%%   msg := recvFrom[SelfSigned[DataNodeResponse]]({_ ∈ %b_Set})
%%   match msg:
%%     case (SelfSigned(_,pay)):
%%       _ := pay
%% proctype (b:b_Set):
%%   anf0 := (DNS({@HashMap[BlobId ByteString]}))
%%   st0 := anf0
%%   while true:
%%     ds_dfg0 := recv[Tuple[ProcessId DataNodeAPI]]
%%     match ds_dfg0:
%%       case (Tuple(who,msg)):
%%         match msg:
%%           case (AddBlob(bn,blob)):
%%             match {@Maybe[ByteString]}:
%%               case Nothing:
%%                 anf2 := case st0 of
%%                           DNS ds_dZyc ->
%%                             (DNS({@HashMap[String ByteString]}))
%%                 anf1 := (ReplyMsg(OK,anf2))
%%                 reply := anf1
%%               case (Just(bdata)):
%%                 anf3 := (ReplyMsg(BlobExists,st0))
%%                 reply := anf3
%%           case (GetBlob(bid)):
%%             match {@Maybe[ByteString]}:
%%               case Nothing:
%%                 anf4 := (ReplyMsg(BlobNotFound,st0))
%%                 reply := anf4
%%               case (Just(bdata)):
%%                 anf6 := (BlobData(bdata))
%%                 anf5 := (ReplyMsg(anf6,st0))
%%                 reply := anf5
%%         match reply:
%%           case (ReplyMsg(r,s')):
%%             anf7 := (SelfSigned(b,r))
%%             send[SelfSigned[DataNodeResponse]](who,anf7)
%%           case (NoReply(s')):
%%             skip
%%     st0 := s'
%%     continue
rewrite_query(T,Rem,[],Name) :-
        Rem=skip,
        T=par([ seq([ cases(a, ndet, [ case(a, cstr__C#(Ds_d8vq), cases(a, Ds_d8vq, [ case(a, cstr__#vv, seq([ assign(a, anf0, cstr__GetBlob(ndet)),
                                                                                                     assign(a, rpc, anf0)])),
                                                                            case(a, cstr__#vv, seq([ assign(a, anf1, cstr__AddBlob(ndet, ndet)),
                                                                                                     assign(a, rpc, anf1)]))], default(a, assign(a, rpc, ndet))))]),
            assign(a, anf2, nonDet(a, b_Set)),
            send(a, e_var(anf2), tyCon(ty__DataNodeAPI), rpc),
            recv(a, nonDet(a, b_Set), type(tyCon(ty__SelfSigned, tyCon(ty__DataNodeResponse))), msg),
            cases(a, msg, [ case(a, cstr__SelfSigned(_, Pay), assign(a, _, Pay))])]),
      sym(b, set(b_Set), seq([ assign(b, anf0, cstr__DNS(ndet)),
                               assign(b, st0, anf0),
                               while(b, seq([ recv(b, type(tyCon(ty__Tuple, tyCon(ty__ProcessId), tyCon(ty__DataNodeAPI))), ds_dfg0),
                                              cases(b, ds_dfg0, [ case(b, cstr__Tuple(Who, Msg), seq([ cases(b, Msg, [ case(b, cstr__AddBlob(Bn, Blob), cases(b, ndet, [ case(b, cstr__Nothing, seq([ cases(b, st0, [ case(b, cstr__DNS(Ds_dZyc), assign(b, anf2, cstr__DNS(ndet)))]),
                                                                                                                                                                                                      assign(b, anf1, cstr__ReplyMsg(cstr__OK, anf2)),
                                                                                                                                                                                                      assign(b, reply, anf1)])),
                                                                                                                                                                         case(b, cstr__Just(Bdata), seq([ assign(b, anf3, cstr__ReplyMsg(cstr__BlobExists, st0)),
                                                                                                                                                                                                          assign(b, reply, anf3)]))])),
                                                                                                                       case(b, cstr__GetBlob(Bid), cases(b, ndet, [ case(b, cstr__Nothing, seq([ assign(b, anf4, cstr__ReplyMsg(cstr__BlobNotFound, st0)),
                                                                                                                                                                                                 assign(b, reply, anf4)])),
                                                                                                                                                                    case(b, cstr__Just(Bdata), seq([ assign(b, anf6, cstr__BlobData(Bdata)),
                                                                                                                                                                                                     assign(b, anf5, cstr__ReplyMsg(anf6, st0)),
                                                                                                                                                                                                     assign(b, reply, anf5)]))]))]),
                                                                                                       cases(b, reply, [ case(b, cstr__ReplyMsg(R, S_), seq([ assign(b, anf7, cstr__SelfSigned(b, R)),
                                                                                                                                                              send(b, e_pid(Who), tyCon(ty__SelfSigned, tyCon(ty__DataNodeResponse)), anf7)])),
                                                                                                                         case(b, cstr__NoReply(S_), skip)])]))]),
                                              assign(b, st0, s_),
                                              continue]))]))]),
        Name='DN-Scenario1'.