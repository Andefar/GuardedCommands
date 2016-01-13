//module Setup
// Michael R. Hansen 05-01-2016

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

// You must revise this path
System.IO.Directory.SetCurrentDirectory localPath;