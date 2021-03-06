﻿namespace GuardedCommands.Frontend
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
   
   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                       | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]           -> ITyp
                                       | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["=";"<";">";"<>";"<="] -> BTyp
                                       | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&";"="]              -> BTyp 
                                       | _ -> failwith("illegal/illtyped dyadic expression: " + f)

/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv = 
         function 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t  
         | AIndex(acc, e) -> let aTyp = match (tcA gtenv ltenv acc) with
                                          | ATyp (t,None) -> t
                                          | ATyp (t,Some(i)) when i>=0 -> t
                                          | _ -> failwith "tcA: can't access negative index at array"
                             if (aTyp <> tcE gtenv ltenv e) then 
                                 failwith "tcA: types of array and access type, does not match" 
                             aTyp

         | ADeref e       -> failwith "tcA: pointer dereferencing not supported yes"
 
/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv topt s = function                           
                         | PrintLn e        -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e)       -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                               then ()
                                               else failwith "tcS: illegal typed assignment"
                         | Alt(GC gc)       -> List.iter (tcGC gtenv ltenv topt s) gc 
                         | Do(GC gc)        -> List.iter (tcGC gtenv ltenv topt s) gc                                
                         | Block(decs,stms) -> if (s = "funproc") then
                                                  ignore(varList [] decs)
                                               let l = tcGDecs ltenv decs
                                               List.iter (tcS gtenv l topt s) stms
                         | Return(Some e)   -> match topt with 
                                                | None   -> failwith "tcS: Return statement is not allowed here"
                                                | Some t -> if (tcE gtenv ltenv e = t) then ()
                                                            else failwith ("tcS: expected return type mismatch " + toStringT ([t]) + " as return, but got " + toStringT ([tcE gtenv ltenv e])) 
                         | Return(None)     -> failwith "tcS: return statement need something to return"
                         | Call(p,stms)     -> ignore(checkParams p stms gtenv ltenv "proc") 

   and tcGC gtenv ltenv topt s (ex,stms) = 
                       if (tcE gtenv ltenv ex = BTyp) then
                            List.iter (tcS gtenv ltenv topt s) stms
                       else failwith "Guarded command expects boolean"

   and tcGDec gtenv = function  
         | VarDec(t,s) -> Map.add s t gtenv 
         | FunDec(topt,f, varDecs, stm) -> tcFun topt f varDecs stm gtenv

   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv

/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty None "") stms


// ------------------------------------------ Our own implemented functions ------------------------------------------

    // used to check function/procedures equality of call types and originally declared parameter types
   and checkParams f elist gtenv ltenv s = 
       let checkTypes paramTypList = let callTypes = List.map (tcE gtenv ltenv) elist
                                     let equal = List.forall2 (fun x y -> match (x,y) with 
                                                                          | (ATyp (t1,_),ATyp(t2,_)) -> t1=t2
                                                                          | (t1,t2) -> t1=t2) 
                                                               callTypes paramTypList
                                     if(not equal) then 
                                          failwith ("type missmatch from call from params. Call " + f 
                                          + " doesn't match the declaration of function" + "\n calltypes:" + (toStringT callTypes) 
                                          + "\n  paramTypList" + (toStringT paramTypList))
       
       let var = try Map.find f gtenv
                 with ex -> failwith ("checkParams: " + string ex) 

       match var with
           | FTyp(types,Some t) -> if (s<>"fun") then failwith "checkParams: function is not used the right way"
                                   checkTypes types
                                   t
           | FTyp(types,None)   -> if (s<>"proc") then failwith "checkParams: procedure is not used the right way"
                                   checkTypes types
                                   FTyp(types,None)
           | _                  -> failwith ("tcE: no function or procedure with this name: " + f)
   
   // varList is used to get a (string*Typ)list, because of dec list mismatch and when checking for FunDec declarations
   and varList l = function
      | []              -> l
      | VarDec(t,s)::r  -> varList (l@[(s,t)]) r
      | _               -> failwith "Functions are not allowed to be declared here"

   and tcFun topt f dec stm gtenv = 
       
         let paramVars = varList [] dec                         
         let types = snd (List.unzip paramVars)                            // parameter types for function

         // checking for duplicate parameters
         if (not (paramVars = [])) then
            let unzipped = fst (List.unzip paramVars)
            if (List.forall (fun name -> 1 < (List.fold (fun state elem -> if (elem = name) then state+1 else state) 0 unzipped)) unzipped) then 
                  failwith "tcFun: duplicate function parameter"

         // check statements with new gtenv and temp. ltenv (incl. check of return types)
         let newGtenv = Map.add f (FTyp(types,topt)) gtenv                  // put the function into global here, so that function can see itself
         let mutable argVars = Map.ofList paramVars

         // the string "fundec" is giving, so that the type check of statements know that where are in a function/procedure
         // it's not allowed to make function/procedure in itself.
         tcS newGtenv argVars topt "funproc" stm                            
         
         // test if it include a return statement at all
         if (topt<>None && not (hasReturnStm stm)) then 
            failwith ("tcFun: The function \"" + f + "\" doesn't have return statement or is not sure to return something cause of ALT or DO statements")
         
         newGtenv             // returns Map<string, typ>

   and hasReturnStm = function
       | Block(_,stms) -> List.exists hasReturnStm stms
       | Alt(GC gc)  -> List.forall (fun (ex,stms) -> List.exists hasReturnStm stms) gc
       | Return(_)-> true
       | _ -> false

   // toString function and aux funtion used to print types in failwith instead og normal print
   and toStringT' s = function
      | []                 -> s
      | ITyp::r            -> toStringT' (s + " ITyp") r 
      | BTyp::r            -> toStringT' (s + " BTyp") r
      | PTyp(_)::r         -> toStringT' (s + " PTyp") r
      | ATyp(t,Some i)::r  -> toStringT' (s + " (ATyp," + toStringT [t] + ", " + string(i) + ")") r
      | ATyp(t,None)::r    -> toStringT' (s + " (ATyp," + toStringT [t] + ", null)") r
      | FTyp(t,_)::r       -> toStringT' (s + " FTyp with " + (toStringT t)) r

   and toStringT l = toStringT' "" l
