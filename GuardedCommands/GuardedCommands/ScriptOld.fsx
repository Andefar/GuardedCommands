﻿// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file 
// The first three are:
let localPath = @"C:\Users\Silas\Dropbox\5. Semester\02257 - Anvendt funktionsprogrammering\Project 2\GuardedCommands\GuardedCommands\GuardedCommands\"
#r @".\bin\Debug\FSharp.PowerPack.dll";
#r @".\bin\Debug\Machine.dll";
#r @".\bin\Debug\\VirtualMachine.dll";

//#r @(String.concat "" [localPath;@"\bin\Debug\FSharp.PowerPack.dll"]);
//#r @".\bin\Debug\Machine.dll";
//#r @".\bin\Debug\\VirtualMachine.dll";

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

// You must revise this path
System.IO.Directory.SetCurrentDirectory localPath;

// The Ex0.gc example:
//let ex0Tree = parseFromFile "Ex0.gc";;
//let _ = tcP ex0Tree;;
//let ex0Code = CP ex0Tree;; 
//let _ = go ex0Tree;;
//let _ = goTrace ex0Tree;;

// Parsing of Ex1.gc
//let ex1Tree = parseFromFile "Ex1.gc";; 

// -- is typechecked as follows:
//let _ = tcP ex1Tree;;

// obtain symbolic code:
//let ex1Code = CP ex1Tree;; 

// -- is executed with trace as follows:
//let stack = goTrace ex1Tree;;

// -- is executed as follows (no trace):
//let sameStack = go ex1Tree;;

// "All in one" parse from file, type check, compile and run 

//let _ = exec "Ex1.gc";;
//let _ = exec "Ex2.gc";;
//ignore(exec "Ex5.gc")

// Only typechecks

#load "TypeCheck.fs"
"fun_test1.gc" |> parseFromFile
"fun_test1.gc" |> parseFromFile |> tcP

#load "CodeGen.fs"
"Ex5.gc" |> parseFromFile |> CP

let exXTree = parseFromFile "if_test1.gc";;
let _ = tcP exXTree;;
let exXCode = CP exXTree;; 
let _ = go exXTree;;
let _ = goTrace exXTree;;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["Ex1.gc"; "Ex2.gc"];;

// All programs relating to the basic version can be parsed:
let pts = List.map parseFromFile ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;

// The parse tree for Ex3.gc
List.item 2 pts;

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["Ex1.gc"; "Ex2.gc";"Ex3.gc"; "Ex4.gc"; "Ex5.gc"; "Ex6.gc"; "Skip.gc"];;
(*

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;

// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

*)