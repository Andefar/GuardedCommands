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
         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"*"; "="; "&&";"-";"<";">";"<>";"<="]        
                                -> tcDyadic gtenv ltenv f e1 e2 
         | Apply(f,elist)       -> checkParams f elist gtenv ltenv "fun"
         | _                    -> failwith "tcE: not supported yet"
   
   and checkParams f elist gtenv ltenv s = 
       let checkTypes paramTypList = let callTypes = List.map (tcE gtenv ltenv) elist
                                     printfn "%A" (callTypes)
                                     printfn "%A" (paramTypList)
                                     if (callTypes <> paramTypList) then failwith ("tcE: checkParams fail, types from call from " + f + " doesn't match the declaration of function" + "\n calltypes:" + (toStringT callTypes) + "\n  paramTypList" + (toStringT paramTypList))
       try
          match Map.find f gtenv with
           | FTyp(types,Some t) -> if (s<>"fun") then failwith "checkParams: function is not used the right way"
                                   checkTypes types
                                   t
           | FTyp(types,None)   -> if (s<>"proc") then failwith "checkParams: procedure is not used the right way"
                                   checkTypes types
                                   FTyp(types,None)
           | _                  -> failwith ("tcE: no function or procedure with this name: " + f)
       with
           | ex -> failwith ("checkParams: kender muligvis ikke function/procedure \"" + f + "\". Fanget exception" + string(ex))
               
   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                       | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]           -> ITyp
                                       | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["=";"<";">";"<>";"<="] -> BTyp
                                       | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]              -> BTyp 
                                       | _ -> failwith("illegal/illtyped dyadic expression: " + f)

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
                         | PrintLn e       -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e)      -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                              then ()
                                              else failwith "illtyped assignment"
                         | Alt(GC gc)       -> List.iter (tcGC gtenv ltenv topt) gc 
                         | Do(GC gc)        -> List.iter (tcGC gtenv ltenv topt) gc                                
                         | Block(decs,stms) -> let l = tcGDecs ltenv decs
                                               printfn "%A" l
                                               printfn "%A" gtenv
                                               List.iter (tcS gtenv l topt) stms
                         | Return(Some e)   -> match topt with 
                                                | None   -> printfn "Type option is %A" topt
                                                            failwith "tcS: Return statement is not allowed here"
                                                | Some t -> if (tcE gtenv ltenv e = t) then ()
                                                            else failwith ("tcS: expected type " + toStringT ([t]) + " as return, but got " + toStringT ([tcE gtenv ltenv e])) 
                         | Return(None)     -> failwith "tcS: return statement need something to return"
                         | Call(p,stms)     -> ignore(checkParams p stms gtenv ltenv "proc") 
   
   and tcGC gtenv ltenv topt (ex,stms) = 
                       if (tcE gtenv ltenv ex = BTyp) then
                            List.iter (tcS gtenv ltenv topt) stms
                       else failwith "GC type check fail"

   and tcGDec gtenv = function  
         | VarDec(t,s)                  -> Map.add s t gtenv
         | FunDec(topt,f, varDecs, stm) -> tcFun topt f varDecs stm gtenv

   and tcFun topt f dec stm gtenv = 
         // used to get a (string*Typ)list, because of dec list mismatch, only var is allowed
         let rec fstList l = function
            | [] -> l
            | VarDec(t,s)::r -> fstList (l@[(s,t)]) r
            | _ -> failwith "tcFun: Functions is not allowed"
         
         let loc = fstList [] dec 
         let types = snd (List.unzip loc)                            // parameter types for function
         printfn "types for function %A is %A" f types

         // checking for duplicate parameters
         if (not (loc = [])) then
            let unzipped = fst (List.unzip loc)
            if (List.forall (fun name -> 1 < (List.fold (fun state elem -> if (elem = name) then state+1 else state) 0 unzipped)) unzipped) then 
                  failwith "tcFun: duplicate function parameter"

         // check statements with new gtenv and temp. ltenv (incl. check of return types)
         let newGtenv = Map.add f (FTyp(types,topt)) gtenv                  // put the function into global here, so that function can see itself
         let mutable argVars = Map.ofList loc
         printfn "Global: %A" newGtenv
         printfn "Local: %A" argVars
         tcS newGtenv argVars topt stm
         
         // test if it include a return statement at all
         if (topt<>None && not (hasReturnStm stm)) then 
            failwith ("tcFun: The function \"" + f + "\" doesn't have return statement or is not sure to return something cause of ALT or DO statements")
         
         // returns Map<string, typ>
         newGtenv

   and hasReturnStm = function
       | Block(_,stms) -> List.exists hasReturnStm stms
       | Alt(GC gc)  -> List.forall (fun (ex,stms) -> List.exists hasReturnStm stms) gc
       | Return(_)-> true
       | _ -> false

   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty None) stms

   and toStringT' s = function
      | [] -> s
      | ITyp::r -> toStringT' (s + " ITyp") r 
      | BTyp::r -> toStringT' (s + " BTyp") r
      | _::r    -> toStringT' s r

   and toStringT l = toStringT' "" l