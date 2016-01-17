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