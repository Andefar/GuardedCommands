// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file 
// The first three are:
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
#r @".\bin\Debug\FSharp.PowerPack.dll";
#r @".\bin\Debug\Machine.dll";
#r @".\bin\Debug\\VirtualMachine.dll";

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration
open ParserUtil
open CompilerUtil
open Machine
open VirtualMachine

System.IO.Directory.SetCurrentDirectory localPath;

let testFail fileName expected = 
   try
      fileName |> exec
   with
    | ex when string(ex).Contains(expected) -> ()
    | ex -> failwith (fileName + " didn't cast the expected exception, but it was " + string(ex))

// testen for some failwiths
testFail "test_fail1.gc" "tcS: illegal typed assignment"
testFail "test_fail2.gc" "Guarded command expects boolean"
testFail "test_fail3.gc" "tcS: expected return type mismatch"
testFail "test_fail4.gc" "tcFun: duplicate function parameter"
testFail "test_fail5.gc" "Functions are not allowed to be declared here"
testFail "test_fail6.gc" "Functions are not allowed to be declared here"
testFail "test_fail7.gc" "doesn't have return statement"
testFail "proc1.gc" "Return statement is not allowed here"
testFail "proc2.gc" "procedure is not used the right way"
testFail "proc3.gc" "The lists had different lengths"

// Some small tests
"fun_test.gc" |> exec  
"proc_test.gc" |> exec  

// ------------------------- List of files our program can run -------------------------

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;

// ------------------------- List of files our program can't run -------------------------

(*
// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;
*)

