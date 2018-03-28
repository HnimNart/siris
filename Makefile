# Makefile for compiling and cleaning up the F# Fasto compiler.
#
# Completely non-clever explicit make targets for every file in the
# Fasto compiler.

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
InterpreterLib=bin/Interpreter.dll
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

$(InterpreterLib): src/Interpreter.fs $(AbSynLib) $(SymTabLib)
	$(fsharpc) -a src/Interpreter.fs -r $(AbSynLib) -r $(SymTabLib) -o $(InterpreterLib)

ReverseProg.dll: ../src/ReverseProg.fs AbSyn.dll
	$(fsharpc) -a ../src/ReverseProg.fs  -r AbSyn.dll -o ReverseProg.dll


$(SirisExe): src/Siris.fsx $(AbSynLib) $(ParserLib) $(LexerLib) $(SymTabLib) $(InterpreterLib) $(ReverseProgLib)
	$(fsharpc) src/Siris.fsx -o $(SirisExe) -r $(AbSynLib) -r $(SymTabLib) -r $(ParserLib) -r $(LexerLib) -r $(InterpreterLib) -r $(ReverseProgLib) -r $(powerpack) -o $(SirisExe)

clean:
	rm -f bin/*.dll bin/*.fs bin/Parser.fsyacc.output bin/Parser.fsi bin/Fasto.exe
	rm -f tests/*.asm tests/*.out-testresult
