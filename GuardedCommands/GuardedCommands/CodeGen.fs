namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE vEnv fEnv = 
       function
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 

       | Apply("-", [e]) -> CE vEnv fEnv e @  [CSTI 0; SWAP; SUB]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]
       | Apply("!",[a])  -> CE vEnv fEnv a @ [NOT]
       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["-";"+"; "*"; "="]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "-"  -> [SUB]
                                          | "*"  -> [MUL]
                                          | "="  -> [EQ] 
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins
       | Apply(f,elist) -> let (labf,retTyp,paraDecs) = Map.find f fEnv
                           let le = List.length elist
                           (List.concat(List.map (fun e -> CE vEnv fEnv e) elist)) @ [CALL(le,labf)]
                                                         
       | _            -> failwith "CE: not supported yet"


/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [GETBP; CSTI addr; ADD]

                               | AIndex(acc, e) -> match acc with 
                                                   | AVar idx -> (CA vEnv fEnv (AVar(idx))) @ [LDI] @ (CE vEnv fEnv e) @ [ADD]
                                                   | _ -> failwith "CA: access aray only with a variable name"

                               | ADeref e       -> failwith "CA: pointer dereferencing not supported yet"

  
(* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> raise (Failure "allocate: array of arrays not permitted")

    | ATyp (t, Some i) -> let newEnv = (Map.add x (kind (fdepth+i), typ) env, (fdepth+i+1))
                          let code = [INCSP i; GETSP; CSTI (i-1); SUB]
                          (newEnv,code)
                          
    | _                -> let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
                          let code = [INCSP 1]
                          (newEnv, code)

                      
/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   let rec CS vEnv fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(acc,e)       -> CA vEnv fEnv acc @ CE vEnv fEnv e @ [STI; INCSP -1]

       | Alt(GC gc)       -> let labEnd = newLabel()
                             List.collect (CGC vEnv fEnv labEnd) gc
                             @ [STOP;Label labEnd]
       | Do(GC gc)        -> let labStart = newLabel()
                             [Label labStart] @
                             List.collect (CGC vEnv fEnv labStart) gc
       | Block([],stms)   -> CSs vEnv fEnv stms

              
       | Block (decL,stmL) -> let rec loopDec decs varEnv =
                                 match decs with 
                                 | []         -> (snd varEnv,[],varEnv)
                                 | dec1::decr -> 
                                   match dec1 with
                                   | FunDec _ -> failwith "func block fundec!"
                                   | VarDec(typ,na) ->
                                     let (env1,code1) = allocate LocVar (typ, na) varEnv
                                     let (depr, coder,newEnv) = loopDec decr env1
                                     (depr, (code1 @ coder), newEnv)
                              let rec loopStm stms varEnv funEnv =
                                 match stms with
                                 | []         -> []
                                 | stm1::stmr -> CS varEnv funEnv stm1 @ (loopStm stmr varEnv funEnv)
                              let (depEnd,decCode,nVEnv) = loopDec decL vEnv
                              let stmCode = loopStm stmL nVEnv fEnv
                              decCode @ stmCode @ [INCSP((snd vEnv) - depEnd)]  
    
       | Return (Some e)  -> (CE vEnv fEnv e) @ [RET (snd vEnv)] 
       
       | Call(p,elist)           -> let (labf,retTyp,paraDecs) = Map.find p fEnv
                                    let le = List.length elist
                                    (List.concat(List.map (fun e -> CE vEnv fEnv e) elist)) @ [CALL(le,labf);INCSP -1]

       | _                -> failwith "CS: this statement is not supported yet"

   and CGC vEnv fEnv lab (ex,stms) =
        let labFalse = newLabel() 
        CE vEnv fEnv ex @ [IFZERO labFalse] @ CSs vEnv fEnv stms @ [GOTO lab;Label labFalse]
   
   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs = 
      let rec addv decs vEnv fEnv = 
         match decs with 
         | []         -> (vEnv, fEnv, [])
         | dec::decr  -> 
         match dec with
         | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                (vEnv2, fEnv2, code1 @ code2) 
         | FunDec (None , f, xs, body) -> addv decr vEnv (Map.add f (newLabel(), None , xs) fEnv)
         | FunDec (tyOpt, f, xs, body) -> addv decr vEnv (Map.add f (newLabel(), tyOpt, xs) fEnv)       
      addv decs (Map.empty, 0) Map.empty
   


   let bindAux (en,fd) dla =
      match dla with
       | VarDec(ty,na) -> ((Map.add na (LocVar fd,ty) en),fd+1)
       | _             -> failwith "bindAux: was expecting varDec only"

   let getEnvDepth paraDecs (gen,fde) = 
      List.fold bindAux (gen,fde) (paraDecs)

/// CP prog gives the code for a program prog
   let CP (P(decs,stms)) = 
      let _ = resetLabels ()
      let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs

      let getFuncCode(tyOpt, f, xs, body) = 
         let (funcLab,_,pars) = Map.find f fEnv
         let (envf, funcDepth) = getEnvDepth pars (gvM,0)
         let funcCode = CS (envf,funcDepth) fEnv body
         let retAdr = ((List.length pars)-1)
         if (tyOpt = None) then 
             [Label funcLab] @ funcCode @ [RET retAdr] @ [STOP]
         else 
             [Label funcLab] @ funcCode @ [RET retAdr]

      let CF =
         List.choose (function
                          | FunDec (rt,nam,arg,s) -> Some (getFuncCode(rt,nam,arg,s))
                          | VarDec _ -> None) decs
         
      initCode @ (CSs gvEnv fEnv stms) @ [STOP] @ (List.concat CF)

