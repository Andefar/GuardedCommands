
// Michael R. Hansen 05-01-2016

// You must revise 4 pathes occurring in this file 
// The first three are:
#r @"./bin/Debug/FSharp.PowerPack.dll";;
#r @"./bin/Debug/Machine.dll";
#r @"./bin/Debug/VirtualMachine.dll";

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
System.IO.Directory.SetCurrentDirectory @"/Users/AndreasLauritzen/Documents/Skole/DTU/GuardedCommands/GuardedCommands";;

let _ = exec "Ex3.gc"
