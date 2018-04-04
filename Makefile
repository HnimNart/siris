# Makefile for compiling and cleaning up the Siris compiler.
# Operating system intended to make OSX work, when running mkbundle
OS=$(shell uname -s)
ifeq ($(OS),Darwin)
  export AS=as -arch i386
  export CC=cc -arch i386 -framework CoreFoundation -lobjc -liconv
endif

.PHONY: all clean

fslex=mono lib/fslex.exe
fsyacc=mono lib/fsyacc.exe
powerpack=lib/FSharp.PowerPack.dll
fsharpc=fsharpc --nologo

LexerGen=bin/Lexer.fs
ParserGen=bin/Parser.fs
AbSynLib=bin/AbSyn.dll
ParserLib=bin/Parser.dll
LexerLib=bin/Lexer.dll
SymTabLib=bin/SymTab.dll
StaticCheckerLib=bin/StaticChecker.dll
InterpreterLib=bin/Interpreter.dll
SubstituteLib=bin/Substitute.dll
ReverseProgLib=bin/ReverseProg.dll
SirisExe=bin/Siris.exe

all: $(SirisExe)

$(LexerGen): src/Lexer.fsl
	$(fslex) src/Lexer.fsl -o $(LexerGen)

$(ParserGen): src/Parser.fsp
	$(fsyacc) -v --module Parser src/Parser.fsp -o $(ParserGen)

$(AbSynLib): src/AbSyn.fs
	$(fsharpc) -a src/AbSyn.fs -o $(AbSynLib)

$(ParserLib): $(ParserGen) $(AbSynLib)
	$(fsharpc) -a $(ParserGen) -r $(AbSynLib) -r $(powerpack) -o $(ParserLib)

$(LexerLib): $(LexerGen) $(AbSynLib) $(ParserLib)
	$(fsharpc) -a $(LexerGen) -r $(AbSynLib) -r $(ParserLib) -r $(powerpack) -o $(LexerLib)

$(SymTabLib): src/SymTab.fs
	$(fsharpc) -a src/SymTab.fs -o $(SymTabLib)

$(ReverseProgLib): src/ReverseProg.fs $(AbSynLib)
	$(fsharpc) -a src/ReverseProg.fs  -r $(AbSynLib) -o $(ReverseProgLib)

$(SubstituteLib): src/Substitute.fs $(AbSynLib)
	$(fsharpc) -a src/Substitute.fs  -r $(AbSynLib) -o $(SubstituteLib)

$(StaticCheckerLib): src/StaticChecker.fs $(AbSynLib) $(SymTabLib)
	$(fsharpc) -a src/StaticChecker.fs  -r $(AbSynLib) -r $(SymTabLib) -o $(StaticCheckerLib)

$(InterpreterLib): src/Interpreter.fs $(AbSynLib) $(SymTabLib) $(SubstituteLib) $(StaticCheckerLib)  $(ReverseProgLib)
	$(fsharpc) -a src/Interpreter.fs -r $(AbSynLib) -r $(SymTabLib) -r $(SubstituteLib) -r $(StaticCheckerLib) -r $(ReverseProgLib) -o $(InterpreterLib)

$(SirisExe): src/Siris.fsx $(AbSynLib) $(ParserLib) $(LexerLib) $(SymTabLib) $(SubstituteLib) $(StaticCheckerLib) $(InterpreterLib) $(ReverseProgLib)
	$(fsharpc) src/Siris.fsx -o $(SirisExe) -r $(AbSynLib) -r $(SymTabLib) -r $(ParserLib) -r $(LexerLib)  -r $(StaticCheckerLib)  -r $(SubstituteLib) -r $(InterpreterLib) -r $(ReverseProgLib) -r $(powerpack) -o $(SirisExe)
	 


clean:
	rm -f bin/*.dll bin/*.fs bin/Parser.fsyacc.output bin/Parser.fsi bin/Siris.exe
	rm -f tests/*.out-testresult
