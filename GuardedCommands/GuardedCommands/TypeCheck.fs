namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck = 

/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
   let rec tcE gtenv ltenv = function                            
         | N _                  -> ITyp   
         | B _                  -> BTyp   
         | Access acc           -> tcA gtenv ltenv acc     
                   
         | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-";"!"]  
                                -> tcMonadic gtenv ltenv f e        
         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"*"; "="; "&&";"-"]        
                                -> tcDyadic gtenv ltenv f e1 e2 
         //functions matches here
         | Apply(f,elist)       -> match tcA gtenv ltenv (AVar f) with
                                    | FTyp(t1,t2) -> checkParams (t1,t2) elist gtenv ltenv 
                                    | _      -> failwith ("tcE: no function with this name: " + f)

//                                    match Map.tryFind f ltenv with
//                                       | None   -> match Map.tryFind f gtenv with
//                                                    | None   -> failwith ("no declaration for function: " + f)
//                                                    | Some t -> checkParams t elist gtenv ltenv
//                                       | Some t -> checkParams t elist gtenv ltenv
//                                       checkParams t elist gtenv ltenv
         | _                    -> failwith "tcE: not supported yet"
   
   and checkParams (paramTypList,funcTyp) elist gtenv ltenv = 
       let callTypes = List.collect (fun x -> [tcE gtenv ltenv x]) elist
       if (callTypes <> paramTypList) then failwith "tcE: checkParams fail, type from call doesn't match the declaration"
       
       match funcTyp with
       | None -> failwith "chechkparams: none return not implemented yet"
       | Some t -> t

   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["="]          -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]     -> BTyp 
                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   and tcNaryFunction gtenv ltenv f es = failwith "type check: functions not supported yet"
 
   and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"
      

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv = 
         function 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t            
         | AIndex(acc, e) -> failwith "tcA: array indexing not supported yes"
         | ADeref e       -> failwith "tcA: pointer dereferencing not supported yes"
 

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv = function                           
                         | PrintLn e      -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e)     -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                             then ()
                                             else failwith "illtyped assignment"
                         | Alt(GC gc)     -> List.iter (tcGC gtenv ltenv) gc 
                         | Do(GC gc)      -> List.iter (tcGC gtenv ltenv) gc                                
                         | Block([],stms) -> List.iter (tcS gtenv ltenv) stms
                         | Return(Some e) -> ignore(tcE gtenv ltenv e)
                         | Return(None)   -> failwith "tcS: Return none not implemented yet"
                         | _              -> failwith "tcS: this statement is not supported yet"
   
   and tcGC gtenv ltenv (ex,stms) = 
                       if (tcE gtenv ltenv ex = BTyp) then
                            List.iter (tcS gtenv ltenv) stms
                       else failwith "GC type check fail"

   and tcGDec gtenv = function  
         | VarDec(t,s)               -> Map.add s t gtenv
         //her skal man returnere en ftyp(typ list,some typ) som skal addes til det globe env (gtenv)
         //check samtidig at return e, returnere det samme som "some typ" (functionens return type)
         // check her at params er unikke og at hvert stm er well typed
//                      | FunDec(topt,f, decs, stm)   -> tcFun topt f decs stm gtenv
         | FunDec(topt,f, varDecs, stm) -> tcFun topt f varDecs stm gtenv
//         | _                         -> failwith "type check: function/procedure declarations not yet supported"

   and tcFun topt f (dec:Dec list) stm gtenv = 
         // get local variable from parameter and check statements
         let rec aux2 map = function 
            | []                -> map
            | VarDec(t,s)::rest -> aux2 (Map.add s t map) rest 
            | _                 -> failwith "tcFun: VarDec Fail"
         
         let localVars = aux2 Map.empty dec
         tcS gtenv localVars stm

         let loc = Map.toList localVars

         // checking for duplicate parameters
         if (not (loc = [])) then
            let unzipped = fst (List.unzip loc)
            if (List.forall (fun name -> 1 < (List.fold (fun state elem -> if (elem = name) then state+1 else state) 0 unzipped)) unzipped) then 
                  failwith "tcFun: duplicate function parameter"
         
         // check return type is correct
         match topt with 
            | None   -> failwith "tcFun: got an procedure when matching topt"
            | Some t ->  let rec aux3 = function
                           | Return(Some e) -> (tcE gtenv localVars e) = t
                           | Return(None) -> failwith "tcFun: aux3 fail cause return was none"
                           | Block(_,stms) -> List.forall (aux3) stms
                           | _ -> true 
                         
                         if (not (aux3 stm)) then failwith "tcFun aux3 fail cause of wrong return type check" 
                         
                         // returns Map<function name, FTYP>
                         let types = snd (List.unzip loc)
                         Map.add f (FTyp(types,topt)) gtenv
          
   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

  
/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty) stms

  
