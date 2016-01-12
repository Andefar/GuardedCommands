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
         | Apply(f,elist)       -> match Map.find f gtenv with
                                    | FTyp(t1,t2) -> checkParams f (t1,t2) elist gtenv ltenv 
                                    | _      -> failwith ("tcE: no function with this name: " + f)
         | _                    -> failwith "tcE: not supported yet"
   
   and checkParams f (paramTypList,funcTyp) elist gtenv ltenv = 
       let callTypes = List.map (tcE gtenv ltenv) elist
       printfn "%s" (toStringT callTypes)
       printfn "%s" (toStringT paramTypList)

       if (callTypes <> paramTypList) then failwith ("tcE: checkParams fail, types from call from " + f + " doesn't match the declaration of function" + "\n calltypes:" + (toStringT callTypes) + "\n  paramTypList" + (toStringT paramTypList))
//       /procedure \n calltypes:" + string(callTypes) + "\n  " + string(paramTypList))
       
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
   and tcS gtenv ltenv topt = function                           
                         | PrintLn e      -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e)     -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                             then ()
                                             else failwith "illtyped assignment"
                         | Alt(GC gc)     -> List.iter (tcGC gtenv ltenv topt) gc 
                         | Do(GC gc)      -> List.iter (tcGC gtenv ltenv topt) gc                                
                         | Block([],stms) -> List.iter (tcS gtenv ltenv topt) stms
                         | Return(Some e) -> match topt with 
                                                | None   -> failwith ("tcS: Should not match with this return (Some e) asd asd" + string(topt) + " " + string(e))
                                                | Some t -> if (tcE gtenv ltenv e = t) then ()
                                                            else failwith "tcS: return fail, the return var is not the same as defined" 
                         | Return(None)   -> failwith "tcS: Return none not implemented yet"
                         | _              -> failwith "tcS: this statement is not supported yet"
   
   and tcGC gtenv ltenv topt (ex,stms) = 
                       if (tcE gtenv ltenv ex = BTyp) then
                            List.iter (tcS gtenv ltenv topt) stms
                       else failwith "GC type check fail"

   and tcGDec gtenv = function  
         | VarDec(t,s)                  -> Map.add s t gtenv
         | FunDec(topt,f, varDecs, stm) -> tcFun topt f varDecs stm gtenv

   and tcFun topt f dec stm gtenv = 
         // used to get a (string*Typ)list, because of dec list mismatch
         let rec fstList l = function
            | [] -> l
            | VarDec(t,s)::r -> fstList (l@[(s,t)]) r
            | _ -> failwith "tcFun: all should be VarDec"
         
         let loc = fstList [] dec 

         // checking for duplicate parameters
         if (not (loc = [])) then
            let unzipped = fst (List.unzip loc)
            if (List.forall (fun name -> 1 < (List.fold (fun state elem -> if (elem = name) then state+1 else state) 0 unzipped)) unzipped) then 
                  failwith "tcFun: duplicate function parameter"
         
         // check statements and if return types are correct
         let localVars = Map.ofList loc
         tcS gtenv localVars topt stm
         
         // returns Map<function name, FTYP>
         let types = snd (List.unzip loc)                            // parameter types for function
         printfn "types for function %s is %s" f (toStringT types)
         Map.add f (FTyp(types,topt)) gtenv
          
   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty None) stms

   and toStringT' s = function
      | [] -> s
      | ITyp::r -> toStringT' (" ITyp" + s) r 
      | BTyp::r -> toStringT' (" BTyp" + s) r
      | _::r    -> toStringT' s r

   and toStringT l = toStringT' "" l